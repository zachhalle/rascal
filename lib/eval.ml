open Ast
open Context
open Primitive
open Printf

exception Runtime_error of string

let is_value e =
  match e with
  | Int _ | Float _ | String _
  | Bool _ | Var _ | Quote _
  | Lambda _ -> true
  | List _ -> false

let bind_all context vs es =
  List.fold_left2 (fun context v e -> bind context v e) context vs es

let rec eval context e =
  match e with
  | Int _ | Float _ | String _
  | Bool _ | Quote _ | Lambda _ -> e
  | Var v -> substitute context v
  | List [] -> raise (Runtime_error "Illegal application: empty")
  | List es -> apply context (List.map (eval context) es)

and apply context es =
  match es with
  | [] -> assert false
  | e :: es' ->
    match apply_primitive e es' with
    | Some e' -> e'
    | None ->
      match e with
      | Int _ | Float _ | String _
      | Bool _ | Quote _ | List _ ->
        raise (
          Runtime_error (
            sprintf "Illegal application: head isn't a procedure: %s" (pretty_expr e)))
      | Var v -> apply context (substitute context v :: es')
      | Lambda (vs, e') ->
        let nvs = List.length vs in
        let nes = List.length es' in
        if nvs != nes then
          raise (
            Runtime_error (
              sprintf "Illegal application: arity mismatch: found %d arguments but expected %d" nvs nes))
        else
          eval (bind_all context vs es') e'
            
let eval_stmt context s =
  match s with
  | Expr e -> (context, eval context e)
  | Define (v, e) -> let e' = eval context e in (bind context v e', e')

let eval_prog context prog =
  match prog with
  | [] -> None
  | s :: ss ->
    let context', e = eval_stmt context s in
    let result = List.fold_left (fun (context, _) s -> eval_stmt context s) (context', e) ss in
    Some result

