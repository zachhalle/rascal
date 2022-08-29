open Ast

exception Runtime_error of string

let is_value e =
  match e with
  | Int _ | Float _ | String _
  | Bool _ | Var _ | Quote _
  | Lambda _ -> true
  | List _ -> false

let is_primitive _ = failwith ""

let apply_primitive _ _ = failwith ""

let rec eval context e =
  match e with
  | Int _ | Float _ | String _
  | Bool _ | Var _ | Quote _
  | Lambda _ -> e
  | List [] -> raise (Runtime_error "Illegal empty application.")
  | List es -> apply context (List.map (eval context) es)

and apply _ es =
  match es with
  | [] -> assert false
  | e :: _ ->
    if is_primitive e then
      apply_primitive e es
    else
      match e with
      | Int _ | Float _ | String _
      | Bool _ | Var _ | Quote _
      | List _ -> failwith ""
      | Lambda _ -> failwith ""

let eval_stmt _ = failwith ""

let eval_prog _ = failwith ""
