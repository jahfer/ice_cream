val query_all : f:(Node.t -> bool) -> ?flatten:bool -> Node.t list -> Node.t list
val query : f:(Node.t -> bool) -> ?flatten:bool -> Node.t list -> Node.t option
val is_a : Node.nodetype -> (Node.t -> bool)

val attr_opt : string -> Node.t -> Node.Attr.t option
val string_attr : string -> Node.t -> string
val string_list_attr : string -> Node.t -> string list
val string_attr_opt : string -> Node.t -> string option