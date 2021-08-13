val query_all : f:(Node.t -> bool) -> ?flatten:bool -> Node.t list -> Node.t list
val query : f:(Node.t -> bool) -> ?flatten:bool -> Node.t list -> Node.t option
