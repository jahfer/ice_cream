let rec lazy_query ~f all =
  List.to_seq all
  |> Seq.flat_map (fun x ->
    match Node.children x with
    | Some (children) -> Seq.cons x (lazy_query ~f children)
    | None -> Seq.return x
  )
  |> Seq.filter f

let query_all ~f all =
  List.of_seq @@ lazy_query ~f:f all

let query ~f all =
  let result = lazy_query ~f:f all in
  match result () with
  | Cons (x, _) -> Some x
  | Nil -> None
