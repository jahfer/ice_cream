type t
val make : unit -> t

val get_untyped_tree : t -> Location.t Ast.expression list
val get_declarations : t -> Ast.Declarations.decl list

val import : Filesystem.file -> t -> t
val import_dir : ?filetypes:Filesystem.filetype list -> string -> t -> t
