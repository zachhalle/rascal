exception Runtime_error of string

val primitive_bindings : (string * Ast.expr) list
val apply_primitive : Ast.expr -> Ast.expr list -> Ast.expr option
