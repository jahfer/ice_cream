open Lexing
open EffectHandlers

type filetype = Ruby | RBS | Unsupported

type file = {
  filetype : filetype;
  name : string
}

type 'a witness = ..
type lex_result = LexResult : 'a witness * 'a -> lex_result

type _ eff += LexRuby : lexbuf -> lex_result eff
type _ eff += LexRBS : lexbuf -> lex_result eff
type _ eff += StartLexing : lexbuf -> unit eff

let lex_file file =
  let inx = open_in file.name in
  let lexbuf = Lexing.from_channel inx in
  perform (StartLexing lexbuf);
  let result = match file.filetype with
  | Ruby -> let x = perform (LexRuby lexbuf) in Some x
  | RBS -> let x = perform (LexRBS lexbuf) in Some x
  | Unsupported -> None in
  close_in inx;
  result

let files_of_dir (filetypes : filetype list) (directory : string) : file list =
  let ext_to_filetype f = match (Filename.extension f) with ".rb" -> Ruby | ".rbs" -> RBS | _ -> Unsupported in
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