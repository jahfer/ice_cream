let tap = fun f x -> f x; x

let eval_env ~dir =
  Environment.make ()
  |> Environment.import_dir dir
  |> Environment.get_untyped_tree
  |> Ast_index.create
  |> Query.query_all ~flatten:true ~f:(Query.is_a Ast_index.Method)
  |> tap (fun _ -> print_endline "")
  |> List.iter (fun node ->
    let open Node.Attr in

    let method_name = match (Node.attributes node) |> List.assoc_opt "name" with
    | Some Str_ s -> s
    | _ -> failwith "Unreachable!" in

    let ref_names = match (Node.attributes node) |> List.assoc_opt "parameters" with
    | Some List_ l -> List.filter_map (function | Str_ s -> Some s | _ -> None) l
    | _ -> [] in

    let usages = (Node.children node)
    |> Option.get
    |> Query.query_all ~flatten:true ~f:(Query.is_a Ast_index.Ref)
    |> List.filter (fun x -> match (Node.attributes x) |> List.assoc_opt "name" with
    | Some (Str_ name) -> List.mem name ref_names
    | Some _ -> failwith "Unreachable!"
    | None -> false) in
    
    List.iter (fun x -> 
      match (Node.attributes x) |> List.assoc_opt "name" with
      | Some (Str_ name) ->
        Printf.printf "In method `#%s`, param `%s` used at: %s\n%s"
          method_name
          name
          (Location.loc_as_docstr (Node.location x))
          (Location.loc_as_string (Node.location x))
      | _ -> failwith "Unreachable!"
    ) usages
  )
  (* |> List.iter (fun node ->
    print_endline @@ "# Original code:";
    print_endline @@ Location.loc_as_string (Node.location node);
    print_endline @@ "# RBS:";
    print_endline @@ Node.to_rbs node;
    print_endline @@ "\n# AST node:";
    print_endline @@ Node.pretty_print node;
    print_endline "\n==========================\n";
  ) *)

let check_env ~dir =
  let result = Environment.make ()
  |> Environment.import_dir ~filetypes:[Filesystem.RBS] dir
  |> Environment.get_declarations
  |> List.map Ast.Declarations.string_of_decl
  in print_endline @@ String.concat "\n" result