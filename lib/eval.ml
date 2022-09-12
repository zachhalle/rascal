open Ast
open Context
open Primitive
open Printf

exception Runtime_error of string

let is_value e =
  match e with
  | Int _ | Float _ | String _
  | Bool _ | Var _ | Quote _
  | Lambda _ | Primitive _ | Closure _ -> true
  | List _ | If _ | Fix _ | Let _ | Let_rec _ -> false

let bind_all context (vs, es) =
  List.fold_left2 (fun context v e -> bind context v e) context vs es

let unzip xys =
  let (xs, ys) = List.fold_left (fun (xs, ys) (x, y)-> (x :: xs, y :: ys)) ([], []) xys in
  (List.rev xs, List.rev ys)

let free_variables e =
  let open Set.Make(String) in
  let rec loop bound free e =
    match e with
    | Int _ | Float _ | String _
    | Bool _ | Primitive _ | Quote _ -> free
    | Closure _ -> (* should never introduce any free vars *) free
    | If (e1, e2, e3) ->
      let free' = loop bound free e1 in
      let free'' = loop bound free' e2 in
      loop bound free'' e3
    | Fix (v, e') -> loop (add v bound) free e'
    | List es -> List.fold_left (fun free e -> loop bound free e) free es
    | Let (bindings, e') ->
      let (bound', free') =
        List.fold_left
          (fun (bound, free) (v, e) -> (add v bound, loop bound free e))
          (bound, free)
          bindings
      in
      loop bound' free' e'
    | Let_rec (bindings, e') ->
      let (bound', free') =
        List.fold_left
          (fun (bound, free) (v, e) -> let bound' = add v bound in (bound', loop bound' free e))
          (bound, free)
          bindings
      in
      loop bound' free' e'
    | Lambda (vs, e') ->
      let bound' = List.fold_left (fun bound v -> add v bound) bound vs in
      loop bound' free e'
    | Var v -> if mem v bound then free else add v free
  in
  let list_of_set s = fold (fun v vs -> v :: vs) s [] in
  let free = loop empty empty e in
  list_of_set free

let rec eval context e =
  match e with
  | Int _ | Float _ | String _
  | Bool _ | Quote _ | Closure _
  | Primitive _ -> e
  | Lambda (vs, e') -> Closure (List.map (fun v -> (v, substitute context v)) (free_variables vs), vs, e')
  | Var v -> 
    begin try eval context (substitute context v) with
    | Undefined_variable v -> raise (Runtime_error (sprintf "Undefined variable: %s" v))
    end
  | Fix (v, e) -> eval (bind context v (Fix (v, e))) e
  | Let (bindings, e) ->
    let context' = List.fold_left (fun context (v, e) -> bind context v (eval context e)) context bindings in
    eval context' e
  | Let_rec (bindings, e) ->
    let context' =
      List.fold_left
        (fun context (v, e) -> bind context v (eval context (Fix (v, e))))
        context 
        bindings
    in
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
    match e with
    | If _ | Var _ | Fix _ | Let _ | Let_rec _ | List _ | Lambda _ -> assert false
    | Int _ | Float _ | String _
    | Bool _ | Quote _ ->
      raise (
        Runtime_error (
          sprintf "Illegal application: head isn't a procedure: %s" (pretty_expr e)))
    | Primitive primitive -> 
      begin try apply_primitive primitive es' with
      | Primitive.Runtime_error msg -> raise (Runtime_error msg)
      end
    | Closure (bindings, vs, e') ->
      let nvs = List.length vs in
      let nes = List.length es' in
      if nvs != nes then
        raise (
          Runtime_error (
            sprintf "Illegal application: arity mismatch: found %d arguments but expected %d" nes nvs))
      else
        eval (bind_all (bind_all context (vs, es')) (unzip bindings)) e'
                
let eval_stmt context s =
  match s with
  | Expr e -> (context, eval context e)
  | Define (v, e) -> let e' = eval context e in (bind context v e', e')
  | Define_rec (v, e) -> let e' = eval context (Fix (v, e)) in (bind context v e', e')

let eval_prog context prog =
  match prog with
  | [] -> None
  | s :: ss ->
    let context', e = eval_stmt context s in
    let result = List.fold_left (fun (context, _) s -> eval_stmt context s) (context', e) ss in
    Some result

