type num = Float of float | Int of int

type expr =
  | Num of num
  | Bool of bool
  | String of string
  | Var of string
  | Primitive of string
  | Fix of string * expr
  | Let of ((string * expr) list) * expr
  | Let_rec of ((string * expr) list) * expr
  | If of expr * expr * expr
  | Quote of expr
  | Lambda of string list * expr
  | Closure of (string * expr) list * string list * expr
  | List of expr list

type stmt =
  | Define of string * expr
  | Define_rec of string * expr
  | Expr of expr

let rec pprint_expr e =
  let open PPrint in
  match e with
  | Num (Int i) -> string (string_of_int i)
  | Num (Float f) -> string (string_of_float f)
  | String s -> enclose (char '\"') (char '\"') (string s)
  | Bool b -> string (string_of_bool b)
  | Var v -> string v
  | Primitive v -> string v
  | Fix (v, e) -> flow (break 1) [string "(fix"; string v; pprint_expr e] ^^ string ")"
  | Quote e -> char '\'' ^^ pprint_expr e
  | Let (bindings, e) ->
    let pprint_binding (v, e) = enclose lparen rparen (flow (break 1) [string v; pprint_expr e]) in
    flow (break 1) [
      string "(let";
      enclose lparen rparen (flow_map (break 1) pprint_binding bindings);
      pprint_expr e]
    ^^ string ")"
  | Let_rec (bindings, e) ->
    let pprint_binding (v, e) = enclose lparen rparen (flow (break 1) [string v; pprint_expr e]) in
    flow (break 1) [
      string "(let-rec";
      enclose lparen rparen (flow_map (break 1) pprint_binding bindings);
      pprint_expr e]
    ^^ string ")"
  | If (e1, e2, e3) ->
    flow (break 1) [
      string "(if";
      pprint_expr e1;
      pprint_expr e2;
      pprint_expr e3]
    ^^ string ")"
  | Lambda (vs, e) | Closure (_, vs, e) -> 
    flow (break 1) [
      string "(lambda"; 
      enclose lparen rparen (flow_map (break 1) string vs);
      pprint_expr e] 
    ^^ string ")"
  | List es -> enclose lparen rparen (flow_map (break 1) pprint_expr es)

let pprint_stmt s =
  let open PPrint in
  match s with
  | Define (v, e) -> flow (break 1) [string "(define"; string v; pprint_expr e] ^^ string ")"
  | Define_rec (v, e) -> flow (break 1) [string "(define-rec"; string v; pprint_expr e] ^^ string ")"
  | Expr e -> pprint_expr e

let pprint_prog p =
  let open PPrint in
  flow_map hardline pprint_stmt p

let show_document doc =
  let open PPrint.ToBuffer in
  let rfrac = 0.85 in
  let width = 100 in
  let buffer = Buffer.create 10 in
  pretty rfrac width buffer doc;
  Buffer.contents buffer

let pretty_expr e = show_document (pprint_expr e)

let pretty_stmt s = show_document (pprint_stmt s)

let pretty_prog p = show_document (pprint_prog p)
