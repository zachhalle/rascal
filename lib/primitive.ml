module M = Map.Make(String)
open M
open Ast
open Printf

type primitives = (Ast.expr list -> Ast.expr) M.t

exception Runtime_error of string

let [@warning "-8"] primitives : primitives =
  let add_all entries = List.fold_left (fun d (k, v) -> add k v d) empty entries in
  add_all [
    "+", (fun [Int x; Int y] -> Int (x + y));
    "-", (fun [Int x; Int y] -> Int (x - y));
    "*", (fun [Int x; Int y] -> Int (x * y));
    "/", (fun [Int x; Int y] -> Int (x / y));
    "%", (fun [Int x; Int y] -> Int (x mod y));
    "<", (fun [Int x; Int y] -> Bool (x < y));
    ">", (fun [Int x; Int y] -> Bool (x > y));
    "<=", (fun [Int x; Int y] -> Bool (x <= y));
    ">=", (fun [Int x; Int y] -> Bool (x >= y));
    "&&", (fun [Bool a; Bool b] -> Bool (a && b));
    "||", (fun [Bool a; Bool b] -> Bool (a || b));
    "^^", (fun [String s; String t] -> String (s ^ t));
    "cons", (fun [e; Quote (List es)] -> Quote (List (e :: es)));
    "car", (fun [Quote (List (e :: _))] -> e);
    "cdr", (fun [Quote (List (_ :: es))] -> Quote (List es))
  ]

let apply_primitive e es =
  match e with
  | Int _ | Float _ | String _
  | Bool _ | Quote _ | Lambda _
  | List _ -> None
  | Var v -> 
    match find_opt v primitives with
    | None -> None
    | Some primitive -> 
      try Some (primitive es) with
      | Match_failure _ ->
        raise (
          Runtime_error (
            sprintf "Illegal application: illegal arguments to primitive procedure %s" (pretty_expr (List es))))
