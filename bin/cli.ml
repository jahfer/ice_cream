open Cmdliner

let rbt check dir =
  match check with
  | true -> `Ok(Ice_cream.check_env ~dir)
  | _ -> `Ok(Ice_cream.eval_env ~dir)

let dir =
  let doc = "Directory of Ruby files" in
  Arg.(value & opt string (Sys.getcwd ()) & (info ["d"; "dir"] ~doc ~docv:"DIR"))

let check_type_decls =
  let doc = "Validate RBS files" in
  Arg.(value & flag & (info ["c"; "check"] ~doc))

let cmd =
  let doc = "Parse and display Ruby" in
  let exits = Term.default_exits in
  Term.(ret (const rbt $ check_type_decls $ dir)),
  Term.info "rbt" ~version:"1.0.2" ~exits ~doc

let () =
  Term.(exit @@ eval cmd)

