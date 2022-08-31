open Ast
open Primitive
open Map.Make(String)

type context = expr Map.Make(String).t

exception Undefined_variable of string

let really_empty_context = empty

let substitute context var =
  try find var context with Not_found -> raise (Undefined_variable var)

let bind context var expr =
  match expr with
  | Var var' -> add var (substitute context var') context
  | _ -> add var expr context

let bind_all context bindings =
  List.fold_left (fun context (v, e) -> bind context v e) context bindings

let empty_context = bind_all really_empty_context primitive_bindings
