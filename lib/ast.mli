type expr =
  | Int of int
  | Float of float
  | String of string
  | Bool of bool
  | Var of string
  | Quote of expr
  | Lambda of string * expr
  | List of expr list

type stmt =
  | Define of string * expr
  | Expr of expr

val pretty_expr : expr -> string
val pretty_stmt : stmt -> string
val pretty_prog : stmt list -> string
