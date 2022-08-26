type expr =
  | Int of int
  | Float of float
  | String of string
  | Bool of bool
  | Var of string
  | Lambda of string * expr
  | List of expr list
