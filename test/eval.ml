open Rascal.Ast
open Rascal.Context
open Rascal.Eval
open Rascal.Parse
open Printf

let parse lines =
  Option.get (parse_string (String.concat "\n" lines))

let eval_and_print_expr e =
  printf "%s" (pretty_expr (eval empty_context e))

let eval_and_print_prog prog =
  match eval_prog empty_context prog with
  | None -> printf "Runtime error..."
  | Some (_, e) -> printf "%s" (pretty_expr e)

(* values *)

let%expect_test _ =
  eval_and_print_expr (Int 0);
  [%expect {| 0 |}]

let%expect_test _ =
  eval_and_print_expr (Int 100000);
  [%expect {| 100000 |}]

let%expect_test _ =
  eval_and_print_expr (Float 0.);
  [%expect {| 0. |}]

let%expect_test _ =
  eval_and_print_expr (Float 0.000);
  [%expect {| 0. |}]
  
let%expect_test _ =
  eval_and_print_expr (Float 101.101010);
  [%expect {| 101.10101 |}]

let%expect_test _ =
  eval_and_print_expr (Bool true);
  [%expect {| true |}]

let%expect_test _ =
  eval_and_print_expr (Bool false);
  [%expect {| false |}]

let%expect_test _ =
  eval_and_print_expr (Quote (List []));
  [%expect {| '() |}]

let%expect_test _ =
  eval_and_print_expr (Quote (List [Int 0; Int 1; Int 2]));
  [%expect {| '(0 1 2) |}]

let%expect_test _ =
  eval_and_print_expr (Lambda ([], Quote (List [])));
  [%expect {| (lambda () '()) |}]

let%expect_test _ =
  eval_and_print_expr (Lambda (["x"], (List [Var "+"; Var "x"; Int 1])));
  [%expect {| (lambda (x) (+ x 1)) |}]

(* expressions *)

let%expect_test _ =
  let e = parse ["(+ 1 2)"] in
  eval_and_print_prog e;
  [%expect {| 3 |}]
