type expr =
  | Int of int
  | Float of float
  | String of string
  | Bool of bool
  | Var of string
  | Prim of string
  | Quote of expr
  | Lambda of string list * expr
  | List of expr list

type stmt =
  | Define of string * expr
  | Expr of expr

let rec pprint_expr e =
  let open PPrint in
  match e with
  | Int i -> string (string_of_int i)
  | Float f -> string (string_of_float f)
  | String s -> enclose (char '\"') (char '\"') (string s)
  | Bool b -> string (string_of_bool b)
  | Var v -> string v
  | Prim v -> string v
  | Quote e -> char '\'' ^^ pprint_expr e
  | Lambda (vs, e) -> 
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
