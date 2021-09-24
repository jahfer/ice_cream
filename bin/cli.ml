open Cmdliner

let rbt import_types_dir filename =
  `Ok(Ice_cream.load_environment ~filename ~rbs_directory:import_types_dir)

let srcs =
  let doc = "Source file(s) to copy." in
  Arg.(required & ((pos 0 (some string) None)) & (info [] ~doc))

let import_type_defs =
  let doc = "Path to RBS files" in
  Arg.(value & opt string "" & (info ["I"; "import-rbs"] ~doc ~docv:"DIR"))

let cmd =
  let doc = "Parse and display Ruby" in
  let exits = Term.default_exits in
  Term.(ret (const rbt $ import_type_defs $ srcs)),
  Term.info "rbt" ~version:"1.0.2" ~exits ~doc

let () =
  Term.(exit @@ eval cmd)

