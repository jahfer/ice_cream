module type S = sig
  type elt
  type child_elt

  val node_type : elt -> string
  val location : elt -> Location.t
  val children : elt -> child_elt list option
  val attributes : elt -> (string * string) list
  val parent : elt -> child_elt option
end

type t =
  | Node : 'a * (module S with type elt = 'a and type child_elt = t) -> t

module type NodeIntf = sig
  type node := t
  type t

  val node_type : t -> string
  val location : t -> Location.t
  val children : t -> node list option
  val attributes : t -> (string * string) list
  val parent : t -> node option
end

module Make (X : NodeIntf) : S with type child_elt = t and type elt = X.t =
struct
  type elt = X.t
  type child_elt = t

  include X
end

(* Helper methods *)

let node_type : t -> string =
  fun (Node (x, (module M))) -> M.node_type x

let location : t -> Location.t =
  fun (Node (x, (module M))) -> M.location x

let attributes : t -> (string * string) list =
  fun (Node (x, (module M))) -> M.attributes x

let children : t -> t list option =
  fun (Node (x, (module M))) -> M.children x

let parent : t -> t option =
  fun (Node (x, (module M))) -> M.parent x

let rec pretty_print : ?indent:int -> t -> string =
  fun ?indent:(i=0) (Node (x, (module M))) ->
    let j = i * 2 in
    let node_type = M.node_type x in
    let attributes = M.attributes x in
    let attr_string = match attributes with
    | [] -> ""
    | attrs -> let vals = String.concat " "
      (List.map (fun (k,v) -> Printf.sprintf "%s=\"%s\"" k v) attrs) in
      " " ^ vals ^ " " in
    match M.children x with
    | Some (children) ->
      Printf.sprintf "%*s<%s%s>\n%s\n%*s</%s>"
        j ""
        node_type
        attr_string
        (String.concat "\n" (List.map (pretty_print ~indent:(i+1)) children))
        j ""
        node_type
    | None -> Printf.sprintf "%*s<%s%s/>" j "" node_type attr_string
