open Lexing

let lexbuf_of_file : type a. string -> (lexbuf -> a) -> a = fun filename f ->
  let inx = open_in filename in
  let lexbuf = Lexing.from_channel inx in
  let result = f lexbuf in
  close_in inx;
  result

let rbs_files_of_dir (directory : string) : string list =
  match directory with "" -> [] (* no directory specified *) | _ ->
  match Sys.is_directory directory with
  | true -> Sys.readdir directory
    |> Array.to_list
    |> List.filter (fun f -> Filename.extension f = ".rbs")
    |> List.map (Filename.concat directory)
  | _ -> raise (Failure (Printf.sprintf "`%s` is not a directory!" directory))