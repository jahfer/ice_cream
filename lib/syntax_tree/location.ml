open Lexing

type t = {
  id: int;
  start_pos : position;
  end_pos : position;
}

let gen_id () = Oo.id (object end)

let pos_column pos = pos.pos_cnum - pos.pos_bol + 1

let intersects fname lnum col location =
  match location.start_pos.pos_fname = fname with
  | false -> false
  | true ->
  match location.start_pos.pos_lnum <= lnum && location.end_pos.pos_lnum >= lnum with
  | false -> false
  | true ->
  match pos_column location.start_pos <= col && pos_column location.end_pos >= col with
  | false -> false
  | true -> true

let print_position _outc pos =
  Printf.printf "%s:%i:%i" pos.pos_fname pos.pos_lnum (pos_column pos)

let slice_opt ic =
  try Some (input_line ic)
  with End_of_file -> None

let nth_line n filename =
  let ic = open_in filename in
  let rec aux i =
    match slice_opt ic with
    | Some line ->
      if i = n then begin
        close_in ic;
        (line)
      end else aux (succ i)
    | None ->
      close_in ic;
      failwith "end of file reached"
  in
  aux 1

let slice_at_location loc =
  nth_line loc.start_pos.pos_lnum loc.start_pos.pos_fname

let loc_as_docstr loc = Printf.sprintf "%s:%i:%i"
  loc.start_pos.pos_fname
  loc.start_pos.pos_lnum
  (pos_column loc.start_pos)


let loc_as_string loc =
  let buf = Buffer.create 200 in
  let open Printf in
  bprintf buf "%5s...\n" " ";
  bprintf buf "%6d| %s\n" loc.start_pos.pos_lnum (slice_at_location loc);
  bprintf buf "%6s|" " ";
  let offset = pos_column loc.start_pos in
  let width = max 1 (pos_column loc.end_pos - pos_column loc.start_pos) in
  bprintf buf "%*s%s\n" offset " " (String.make width '^');
  Buffer.contents buf
