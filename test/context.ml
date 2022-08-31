open Rascal.Context
open Rascal.Ast
open Printf

let%expect_test _ =
  try ignore (substitute empty_context "x") with
  | Undefined_variable var -> printf "Undefined_variable: %s" var;
  [%expect {| Undefined_variable: x |}]

let%expect_test _ =
  try ignore (bind empty_context "x" (Var "y")) with
  | Undefined_variable var -> printf "Undefined_variable: %s" var;
  [%expect {| Undefined_variable: y |}]

let%expect_test _ =
  try ignore (bind empty_context "x" (Var "x")) with
  | Undefined_variable var -> printf "Undefined_variable: %s" var;
  [%expect {| Undefined_variable: x |}]

let%expect_test _ =
  let context = bind empty_context "x" (Int 0) in
  printf "%s" (pretty_expr (substitute context "x"));
  [%expect {| 0 |}]

let%expect_test _ =
  let context = bind empty_context "v" (Lambda (["x"; "y"; "z"], (List [Var "+"; Var "x"; Var "y"; Var "z"]))) in
  printf "%s" (pretty_expr (substitute context "v"));
  [%expect {| (lambda (x y z) (+ x y z)) |}]

let%expect_test _ =
  let context = bind empty_context "a" (Quote (Var "+")) in
  printf "%s" (pretty_expr (substitute context "a"));
  [%expect {| '+ |}]

let%expect_test _ =
  let context = bind empty_context "x" (Int 1) in
  let context = bind context "y" (Var "x") in
  printf "%s" (pretty_expr (substitute context "y"));
  [%expect {| 1 |}]

let%expect_test _ =
  let context0 = bind empty_context "x" (Int 0) in
  let context1 = bind context0 "y" (Int 1) in
  let context2 = bind context1 "x" (Var "y") in
  printf "context0 = { %s = %s }\n" "x" (pretty_expr (substitute context0 "x"));
  printf "context1 = { %s = %s }\n" "x" (pretty_expr (substitute context1 "x"));
  printf "context1 = { %s = %s }\n" "y" (pretty_expr (substitute context1 "y"));
  printf "context2 = { %s = %s }\n" "x" (pretty_expr (substitute context2 "x"));
  printf "context2 = { %s = %s }\n" "y" (pretty_expr (substitute context2 "y"));
  [%expect {|
    context0 = { x = 0 }
    context1 = { x = 0 }
    context1 = { y = 1 }
    context2 = { x = 1 }
    context2 = { y = 1 } |}]
