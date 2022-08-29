exception Runtime_error of string

val apply_primitive : Ast.expr -> Ast.expr list -> Ast.expr option
