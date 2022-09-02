open Ast
open Context
open Primitive
open Printf

exception Runtime_error of string

let is_value e =
  match e with
  | Int _ | Float _ | String _
  | Bool _ | Var _ | Quote _
  | Lambda _ | Primitive _ -> true
  | List _ | If _ | Let _ -> false

let bind_all context vs es =
  List.fold_left2 (fun context v e -> bind context v e) context vs es

let rec eval context e =
  match e with
  | Int _ | Float _ | String _
  | Bool _ | Quote _ | Lambda _ 
  | Primitive _ -> e
  | Var v -> substitute context v
  | Let (bindings, e) ->
    let context' = List.fold_left (fun context (v, e) -> bind context v (eval context e)) context bindings in
    eval context' e
  | If (e1, e2, e3) ->
    begin match eval context e1 with
    | Bool true -> eval context e2
    | Bool false -> eval context e3
    | e -> 
      raise (
        Runtime_error (
          sprintf "Illegal condition in if-expression: %s" (pretty_expr e)))
    end
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
      | Primitive _ | If _ | Var _ | Let _ -> assert false
      | Int _ | Float _ | String _
      | Bool _ | Quote _ | List _ ->
        raise (
          Runtime_error (
            sprintf "Illegal application: head isn't a procedure: %s" (pretty_expr e)))
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

