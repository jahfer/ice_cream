let tap = fun f x -> f x; x

let dump_nodes index = index |> List.iter @@ fun node ->
  print_endline @@ "# Original code:";
  print_endline @@ Location.loc_as_string (Node.location node);
  print_endline @@ "# RBS:";
  print_endline @@ Node.to_rbs node;
  print_endline @@ "\n# AST node:";
  print_endline @@ Node.pretty_print node;
  print_endline "\n==========================\n"

let param_usages index = let open Ast_index.Type in
  index
  |> Query.(query_all ~f:(is_a Method)) ~flatten:true
  |> tap (fun _ -> print_endline "# Method parameter usage\n")
  |> List.iter @@ fun node ->
    let method_name = Query.string_attr "name" node
    and param_names = Query.string_list_attr "parameter_names" node in

    let usages = node
    |> Node.children
    |> Option.get
    |> Query.(query_all ~f:(is_a Ref)) ~flatten:true
    |> List.filter @@ fun n ->
      List.mem (Query.string_attr "name" n) param_names in

    usages |> List.iter @@ fun n ->
      let name = Query.string_attr "name" n in
      Printf.printf "In method `#%s`, param `%s` used:\n%s\n%s\n"
        method_name
        name
        (Location.loc_as_docstr (Node.location n))
        (Location.loc_as_string (Node.location n))

let unused_params index =
  let open Ast_index.Type in
  let open ANSITerminal in
  let rec remove item = function
  | [] -> []
  | hd :: tl -> if hd = item then tl else hd :: remove item tl in

  index
  |> Query.(query_all ~f:(is_a Method)) ~flatten:true
  |> List.iter @@ fun method_node ->
    let param_names = Query.string_list_attr "parameter_names" method_node in
    let non_usages = method_node
    |> Node.children
    |> Option.get
    |> Query.(query_all ~f:(match_any [is_a Ref; is_a Assignment])) ~flatten:true
    |> List.fold_left (fun unused_params n ->
      match Node.node_type n with
      | Ref -> remove (Query.string_attr "name" n) unused_params
      | Assignment -> remove Query.(n |> node_attr "target" |> string_attr "name") unused_params
      | _ -> failwith "Unreachable!"
    ) param_names in

    if List.length non_usages > 0 then begin
      prerr_string [Bold; red] "\n[UNUSED PARAMS]";
      eprintf [white] " %s\n\n" (Location.loc_as_docstr (Node.location method_node));
      eprintf [Bold; white] "%s\n" (Node.to_rbs method_node);
      List.iter (fun p -> eprintf [white] " \u{2014} `%s` not used\n" p) non_usages
    end

let eval_env ~dir =
  let index = Environment.make ()
  |> Environment.import_dir dir
  |> Environment.get_untyped_tree
  |> Ast_index.create in

  dump_nodes index;
  (* param_usages index; *)
  unused_params index
  
let check_env ~dir =
  let result = Environment.make ()
  |> Environment.import_dir ~filetypes:[Filesystem.RBS] dir
  |> Environment.get_declarations
  |> List.map Ast.Declarations.string_of_decl
  in print_endline @@ String.concat "\n" result