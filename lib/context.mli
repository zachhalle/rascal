type context

exception Undefined_variable of string

val empty_context : context
val bind : context -> string -> Ast.expr -> context
val substitute : context -> string -> Ast.expr
