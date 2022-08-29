open Ast
open Map.Make(String)

type context = expr Map.Make(String).t

exception Undefined_variable of string

let empty_context = empty
let bind context var expr = add var expr context
let substitute context var =
  try find var context with Not_found -> raise (Undefined_variable var)
