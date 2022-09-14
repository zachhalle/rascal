type num = Float of float | Int of int

type expr =
  | Num of num
  | Bool of bool
  | Char of char
  | String of string
  | Var of string
  | Primitive of string
  | Fix of string * expr
  | Let of ((string * expr) list) * expr
  | Let_rec of ((string * expr) list) * expr
  | If of expr * expr * expr
  | Quote of expr
  | Lambda of string list * expr
  | Closure of (string * expr) list * string list * expr
  | List of expr list

type stmt =
  | Define of string * expr
  | Define_rec of string * expr
  | Expr of expr

val pretty_expr : expr -> string
val pretty_stmt : stmt -> string
val pretty_prog : stmt list -> string
