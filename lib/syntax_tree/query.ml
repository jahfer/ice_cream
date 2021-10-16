let rec lazy_query ~f ~flatten all =
  let s = List.to_seq all in

  if flatten then
    Seq.filter f @@ Seq.flat_map (fun x ->
      match Node.children x with
      | Some (children) -> Seq.cons x (lazy_query ~f ~flatten children)
      | None -> Seq.return x
    ) s
  else Seq.filter f s

  let query_all ~f ?flatten:(flatten=false) all =
  List.of_seq @@ lazy_query ~f ~flatten all

let query ~f ?flatten:(flatten=false) all =
  let result = lazy_query ~f ~flatten all in
  match result () with
  | Cons (x, _) -> Some x
  | Nil -> None

let match_all fns = fun (x : Node.t) ->
  List.for_all (fun f -> f x) fns

let match_any fns = fun (x : Node.t) ->
  List.exists (fun f -> f x) fns

let is_a t = (fun node -> (Node.node_type node) = t)

let attr_opt a node = List.assoc_opt a (Node.attributes node)

let string_attr a node = match attr_opt a node with
| Some Node.Attr.Str_ s -> s
| _ -> raise Not_found

let node_attr a node = match attr_opt a node with
| Some Node.Node_ n -> n
| _ -> raise Not_found

let string_attr_opt a node = match attr_opt a node with
| Some Node.Attr.Str_ s -> Some s
| _ -> None

let node_attr_opt a node = match attr_opt a node with
| Some Node.Node_ n -> Some n
| _ -> None

let string_list_attr a node = let open Node.Attr in
match attr_opt a node with
| Some List_ l -> List.filter_map (function | Str_ s -> Some s | _ -> None) l
| _ -> []

let node_list_attr a node = let open Node.Attr in
match attr_opt a node with
| Some List_ l -> List.filter_map (function | Node.Node_ s -> Some s | _ -> None) l
| _ -> []