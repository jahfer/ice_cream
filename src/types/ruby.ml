type literal =
  | THash
  | TBool
  | TFloat
  | TInt
  | TArray of literal
  | TNil
  | TString
  | TSymbol

(* single constraint found by type system *)
type constraint_t =
  | Literal of literal
  | Disjuction of t list
  | SubType of t
  | Member of string * t
  | Method of string * upper_bound list * lower_bound

(* type constraint structure *)
and c = constraint_t list

(* partial type definition *)
and upper_bound = c Constrained_type.upper_bound
and lower_bound = c Constrained_type.lower_bound

(* full type definition *)
and t = c Constrained_type.t