val query_all : f:(Node.t -> bool) -> ?flatten:bool -> Node.t list -> Node.t list
val query : f:(Node.t -> bool) -> ?flatten:bool -> Node.t list -> Node.t option

val is_a : Node.nodetype -> (Node.t -> bool)
val match_any : (Node.t -> bool) list -> Node.t -> bool
val match_all : (Node.t -> bool) list -> Node.t -> bool

val attr_opt : string -> Node.t -> Node.Attr.t option

val string_attr : string -> Node.t -> string
val string_attr_opt : string -> Node.t -> string option
val string_list_attr : string -> Node.t -> string list

val node_attr : string -> Node.t -> Node.t
val node_attr_opt : string -> Node.t -> Node.t option
val node_list_attr : string -> Node.t -> Node.t list