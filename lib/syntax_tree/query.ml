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

let is_a t = (fun node -> (Node.node_type node) = t)