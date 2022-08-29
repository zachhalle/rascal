exception Runtime_error of string

val is_value : Ast.expr -> bool
val eval : Context.context -> Ast.expr -> Ast.expr
val eval_stmt : Context.context -> Ast.stmt -> Context.context * Ast.expr
val eval_prog : Context.context -> Ast.stmt list -> (Context.context * Ast.expr) option
