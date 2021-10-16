let tap = fun f x -> f x; x

let eval_env ~dir =
  let open Ast_index.Type in
  let index = Environment.make ()
  |> Environment.import_dir dir
  |> Environment.get_untyped_tree
  |> Ast_index.create in

  index
  |> List.iter (fun node ->
    print_endline @@ "# Original code:";
    print_endline @@ Location.loc_as_string (Node.location node);
    print_endline @@ "# RBS:";
    print_endline @@ Node.to_rbs node;
    print_endline @@ "\n# AST node:";
    print_endline @@ Node.pretty_print node;
    print_endline "\n==========================\n";
  );

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

let check_env ~dir =
  let result = Environment.make ()
  |> Environment.import_dir ~filetypes:[Filesystem.RBS] dir
  |> Environment.get_declarations
  |> List.map Ast.Declarations.string_of_decl
  in print_endline @@ String.concat "\n" result