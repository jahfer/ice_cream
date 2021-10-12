type nodetype = ..

module Attr = struct
  type t = ..
  type t += Str_ of string
  type t += List_ of t list
end

module type S = sig
  type elt
  type child_elt

  val node_type : elt -> nodetype
  val location : elt -> Location.t
  val children : elt -> child_elt list option
  val attributes : elt -> (string * Attr.t) list
  val parent : elt -> child_elt option
  val to_rbs : elt -> string
end

type t =
  | Node : 'a * (module S with type elt = 'a and type child_elt = t) -> t

type Attr.t += Node_ of t

module type NodeIntf = sig
  type node := t
  type t

  val node_type : t -> nodetype
  val location : t -> Location.t
  val children : t -> node list option
  val attributes : t -> (string * Attr.t) list
  val parent : t -> node option
  val to_rbs : t -> string
end

module Make (X : NodeIntf) : S with type child_elt = t and type elt = X.t =
struct
  type elt = X.t
  type child_elt = t

  include X
end

(* Helper methods *)

let node_type : t -> nodetype =
  fun (Node (x, (module M))) -> M.node_type x

let location : t -> Location.t =
  fun (Node (x, (module M))) -> M.location x

let attributes : t -> (string * Attr.t) list =
  fun (Node (x, (module M))) -> M.attributes x

let children : t -> t list option =
  fun (Node (x, (module M))) -> M.children x

let to_rbs : t -> string =
  fun (Node (x, (module M))) -> M.to_rbs x

(* Nodes can't assign parent to children when
   children are required to define parent! *)
let parent : t -> t option =
  fun (Node (x, (module M))) -> M.parent x

let rec pretty_print : ?indent:int -> t -> string =
  let open ANSITerminal in
  let rec attr_to_str = function
  | Attr.Str_ s -> s
  | Node_ n -> pretty_print n
  | Attr.List_ l -> "[" ^ String.concat ", " (List.map attr_to_str l) ^ "]"
  | _ -> "" in
  fun ?indent:(i=0) (Node (node, (module M))) ->
    let j = i * 2 in
    let attributes = M.attributes node in
    let node_type = match (List.assoc_opt "node_type" attributes) with
    | Some (Attr.Str_ s) -> s
    | _ -> ""
    in
    let attr_string = match attributes with
    | [] -> ""
    | attrs -> let strs = attrs |> List.map (fun (k,v) -> 
      let str_attr = attr_to_str v in
      Printf.sprintf "(%s %s)" (sprintf [cyan] ":%s" k) (sprintf [green] "%s" str_attr)) in
      " " ^ (String.concat " " strs)
    in
    match M.children node with
    | Some (children) ->
      Printf.sprintf "%*s(%s%s\n%s)"
        j ""
        (sprintf [yellow] "%s" node_type)
        attr_string
        (String.concat "\n" (List.map (pretty_print ~indent:(i+1)) children))
    | None -> Printf.sprintf "%*s%s%s%s)" j "" (sprintf [default] "(") (sprintf [yellow] "%s" node_type) attr_string
