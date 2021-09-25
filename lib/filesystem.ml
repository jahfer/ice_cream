open Lexing

type filetype = Ruby | RBS

let lexbuf_of_file : type a. string -> (lexbuf -> a) -> a = fun filename f ->
  let inx = open_in filename in
  let lexbuf = Lexing.from_channel inx in
  let result = f lexbuf in
  close_in inx;
  result

let files_of_dir (filetype : filetype) (directory : string) : string list =
  let ext = match filetype with Ruby -> ".rb" | RBS -> ".rbs" in
  let rec find_files filenames = function
  | dir :: fs when Sys.is_directory dir -> 
    let found_files = Sys.readdir dir
    |> Array.to_list
    |> List.map (Filename.concat dir)
    |> find_files []
    in find_files (List.rev_append filenames found_files) fs
  | f :: fs when Filename.extension f = ext -> find_files (f :: filenames) fs
  | _ :: fs -> find_files filenames fs
  | [] -> filenames
  in find_files [] [directory]