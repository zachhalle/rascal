open Ast
open Context
open Printf

exception Runtime_error of string

let is_value e =
  match e with
  | Int _ | Float _ | String _
  | Bool _ | Var _ | Quote _
  | Lambda _ -> true
  | List _ -> false

let is_primitive _ = false

let apply_primitive _ _ = failwith ""

let bind_all context vs es =
  List.fold_left2 (fun context v e -> bind context v e) context vs es

let rec eval context e =
  match e with
  | Int _ | Float _ | String _
  | Bool _ | Quote _ | Lambda _ -> e
  | Var v -> substitute context v
  | List [] -> raise (Runtime_error "Illegal empty application")
  | List es -> apply context (List.map (eval context) es)

and apply context es =
  match es with
  | [] -> assert false
  | e :: es' ->
    if is_primitive e then
      apply_primitive e es
    else
      match e with
      | Int _ | Float _ | String _
      | Bool _ | Quote _ | List _ ->
        raise (
          Runtime_error (
            sprintf "Tried to apply a value that isn't a procedure: %s" (pretty_expr e)))
      | Var v -> apply context (substitute context v :: es')
      | Lambda (vs, e') ->
        let nvs = List.length vs in
        let nes = List.length es' in
        if nvs != nes then
          raise (Runtime_error (sprintf "Arity mismatch: found %d arguments but expected %d" nvs nes))
        else
          eval (bind_all context vs es') e'
            

let eval_stmt _ = failwith ""

let eval_prog _ = failwith ""
