module M = Map.Make(String)
open M
open Ast
open Printf

type primitives = (Ast.expr list -> Ast.expr) M.t

exception Illegal_argument
exception Runtime_error of string

let rec for_all2 p xs ys =
  match xs, ys with
  | [], [] -> true
  | x :: xs', y :: ys' -> p x y && for_all2 p xs' ys'
  | _, _ -> false

let [@warning "-8"] eq [v; w] =
  let rec binding_structural_eq (v, e) (v', e') = String.equal v v' && structural_eq e e'
  and structural_eq e1 e2 =
    match e1, e2 with
    | Num (Int x), Num (Int y) -> x == y
    | Num (Float x), Num (Float y) -> x == y
    | Bool x, Bool y -> x == y
    | String x, String y -> String.equal x y
    | Var x, Var y -> String.equal x y
    | Primitive x, Primitive y -> String.equal x y
    | Fix (v, e), Fix (v', e') -> String.equal v v' && structural_eq e e'
    | Let (bindings, e), Let (bindings', e') ->
      for_all2 binding_structural_eq bindings bindings' && structural_eq e e'
    | Let_rec (bindings, e), Let_rec (bindings', e') ->
      for_all2 binding_structural_eq bindings bindings' && structural_eq e e'
    | If (e1, e2, e3), If (e1', e2', e3') -> for_all2 structural_eq [e1; e2; e3] [e1'; e2'; e3']
    | Quote e1, Quote e2 -> structural_eq e1 e2
    | Lambda (vs, e), Lambda (vs', e') -> for_all2 String.equal vs vs' && structural_eq e e'
    | List es, List es' -> for_all2 structural_eq es es'
    | _ -> false
  in
  let value_eq v w =
    match v, w with
    | Num (Int x), Num (Int y) -> x == y
    | Num (Float x), Num (Float y) -> x == y
    | Bool x, Bool y -> x == y
    | String x, String y -> String.equal x y
    | Quote e1, Quote e2 -> structural_eq e1 e2
    | _ -> false
  in
  let check_arg v =
    match v with
    | Num (Int _) -> ()
    | Num (Float _) -> ()
    | String _ -> ()
    | Bool _ -> ()
    | Var _ -> assert false
    | Primitive _ -> raise Illegal_argument
    | If _ -> raise Illegal_argument
    | Fix _ -> raise Illegal_argument
    | Let _ -> raise Illegal_argument
    | Let_rec _ -> raise Illegal_argument
    | Quote _ -> ()
    | Lambda _ -> raise Illegal_argument
    | List _ -> raise Illegal_argument
  in
  check_arg v;
  check_arg w;
  Bool (value_eq v w)

let [@warning "-8"] num f g x y =
  match x, y with
  | Num (Int x), Num (Int y) -> Num (Int (f x y))
  | Num (Int x), Num (Float y) -> Num (Float (g (float_of_int x) y))
  | Num (Float x), Num (Int y) -> Num (Float (g x (float_of_int y)))
  | Num (Float x), Num (Float y) -> Num (Float (g x y))

let [@warning "-8"] num_cmp f g x y =
  match x, y with
  | Num (Int x), Num (Int y) -> Bool (f x y)
  | Num (Int x), Num (Float y) -> Bool (g (float_of_int x) y)
  | Num (Float x), Num (Int y) -> Bool (g x (float_of_int y))
  | Num (Float x), Num (Float y) -> Bool (g x y)

let [@warning "-8"] primitives : primitives =
  let add_all entries = List.fold_left (fun d (k, v) -> add k v d) empty entries in
  add_all [
    "+", (List.fold_left (num (+) (+.)) (Num (Int 0)));
    "-", (fun xs -> 
            match xs with 
            | [] -> Num (Int 0)
            | x :: xs' -> List.fold_left (num (-) (-.)) x xs');
    "*", (List.fold_left (num ( * ) ( *. )) (Num (Int 0)));
    "/", (fun xs ->
            match xs with
            | [] -> Num (Int 1)
            | x :: xs' -> List.fold_left (num (/) (/.)) x xs');
    "%", (fun [Num (Int x); Num (Int y)] -> Num (Int (x mod y)));
    "<", (fun [x; y] -> num_cmp (<) (<) x y);
    ">", (fun [x; y] -> num_cmp (>) (>) x y);
    "<=", (fun [x; y] -> num_cmp (<=) (<=) x y);
    ">=", (fun [x; y] -> num_cmp (>=) (>=) x y);
    "=", eq;
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

let apply_primitive v es =
  match find_opt v primitives with
  | None -> raise (Runtime_error (sprintf "Undefined primitive %s" v))
  | Some primitive -> 
    try primitive es with
    | Illegal_argument
    | Match_failure _ ->
      raise (
        Runtime_error (
          sprintf "Illegal application: illegal arguments to primitive procedure %s" (pretty_expr (List es))))
