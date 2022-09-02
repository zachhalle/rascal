module M = Map.Make(String)
open M
open Ast
open Printf

type primitives = (Ast.expr list -> Ast.expr) M.t

exception Runtime_error of string

let [@warning "-8"] primitives : primitives =
  let add_all entries = List.fold_left (fun d (k, v) -> add k v d) empty entries in
  add_all [
    "+", List.fold_left (fun (Int x) (Int y) -> Int (x + y)) (Int 0);
    "-", (fun xs -> 
            match xs with 
            | [] -> Int 0 
            | x :: xs' -> List.fold_left (fun (Int x) (Int y) -> Int (x - y)) x xs');
    "*", List.fold_left (fun (Int x) (Int y) -> Int (x * y)) (Int 1);
    "/", (fun xs ->
            match xs with
            | [] -> Int 1
            | x :: xs' -> List.fold_left (fun (Int x) (Int y) -> (Int (x / y))) x xs');
    "%", (fun [Int x; Int y] -> Int (x mod y));
    "<", (fun [Int x; Int y] -> Bool (x < y));
    ">", (fun [Int x; Int y] -> Bool (x > y));
    "<=", (fun [Int x; Int y] -> Bool (x <= y));
    ">=", (fun [Int x; Int y] -> Bool (x >= y));
    "&&", List.fold_left (fun (Bool x) (Bool y) -> Bool (x && y)) (Bool true);
    "||", List.fold_left (fun (Bool x) (Bool y) -> Bool (x || y)) (Bool false);
    "^^", List.fold_left (fun (String s) (String t) -> String (s ^ t)) (String "");
    "cons", (fun [e; Quote (List es)] -> Quote (List (e :: es)));
    "car", (fun [Quote (List (e :: _))] -> e);
    "cdr", (fun [Quote (List (_ :: es))] -> Quote (List es))
  ]

let primitive_bindings =
  let bindings = ref [] in
  iter (fun prim _ -> bindings := (prim, Primitive prim) :: !bindings) primitives;
  !bindings

let apply_primitive e es =
  match e with
  | Int _ | Float _ | String _
  | Bool _ | Quote _ | Lambda _
  | List _ | Var _ | If _ | Let _ -> None
  | Primitive v -> 
    match find_opt v primitives with
    | None -> None
    | Some primitive -> 
      try Some (primitive es) with
      | Match_failure _ ->
        raise (
          Runtime_error (
            sprintf "Illegal application: illegal arguments to primitive procedure %s" (pretty_expr (List es))))
