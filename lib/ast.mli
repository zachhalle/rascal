type expr =
  | Int of int
  | Float of float
  | String of string
  | Bool of bool
  | Var of string
  | Primitive of string
  | Let of ((string * expr) list) * expr
  | Let_rec of (string * expr) * expr
  | If of expr * expr * expr
  | Quote of expr
  | Lambda of string list * expr
  | List of expr list

type stmt =
  | Define of string * expr
  | Expr of expr

val pretty_expr : expr -> string
val pretty_stmt : stmt -> string
val pretty_prog : stmt list -> string
