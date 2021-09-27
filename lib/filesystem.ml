open Lexing

type filetype = Ruby | RBS | Uunsupported

type file = {
  filetype : filetype;
  name : string
}

let lexbuf_of_file : type a. string -> (lexbuf -> a) -> a = fun filename f ->
  let inx = open_in filename in
  let lexbuf = Lexing.from_channel inx in
  let result = f lexbuf in
  close_in inx;
  result

let files_of_dir (filetypes : filetype list) (directory : string) : file list =
  let ext_to_filetype f = match (Filename.extension f) with ".rb" -> Ruby | ".rbs" -> RBS | _ -> Uunsupported in
  let filename_to_file name = { name; filetype = ext_to_filetype name } in
  let rec find_files files = function
  | dir :: fs when Sys.is_directory dir -> 
    let found_files = Sys.readdir dir
    |> Array.to_list
    |> List.map (Filename.concat dir)
    |> find_files []
    in find_files (List.rev_append files found_files) fs
  | f :: fs when (List.mem (ext_to_filetype f) filetypes) -> find_files ((filename_to_file f) :: files) fs
  | _ :: fs -> find_files files fs
  | [] -> files
  in find_files [] [directory]