let load_environment ~filename ~rbs_directory =
  Environment.make ()
  |> Environment.import_rbs_dir rbs_directory
  |> Environment.import_rb filename
  |> Environment.ast_for_filename filename
  |> Ast_index.create
  |> Query.query_all ~flatten:true ~f:(fun node ->
    (Node.node_type node) = "RefNode"
  )
  |> List.iter (fun node ->
    print_endline @@ "# Original code:";
    print_endline @@ Location.loc_as_string (Node.location node);
    print_endline @@ "# RBS:";
    print_endline @@ Node.to_rbs node;
    print_endline @@ "\n# AST node:";
    print_endline @@ Node.pretty_print node;
    print_endline "\n==========================\n";
  )