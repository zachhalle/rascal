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
  let e = parse ["(+)"] in
  eval_and_print_prog e;
  [%expect {| 0 |}]

let%expect_test _ =
  let e = parse ["(+ -1)"] in
  eval_and_print_prog e;
  [%expect {| -1 |}]

let%expect_test _ =
  let e = parse ["(+ 1 2)"] in
  eval_and_print_prog e;
  [%expect {| 3 |}]

let%expect_test _ =
  let e = parse ["(+ 1 -2 3)"] in
  eval_and_print_prog e;
  [%expect {| 2 |}]

let%expect_test _ =
  let e = parse ["(- 1 -2 3)"] in
  eval_and_print_prog e;
  [%expect {| 0 |}]

let%expect_test _ =
  let e = parse [
    "(define positive? (lambda (d) (> d 0)))";
    "(positive? 1)"]
  in
  eval_and_print_prog e;
  [%expect {| true |}]

let%expect_test _ =
  let e = parse [
    "(define positive? (lambda (d) (> d 0)))";
    "(positive? 0)"]
  in
  eval_and_print_prog e;
  [%expect {| false |}]

let%expect_test _ =
  let e = parse [
    "(define positive? (lambda (d) (> d 0)))";
    "(if (positive? 1) 'mario 'luigi)"
  ] in
  eval_and_print_prog e;
  [%expect {| 'mario |}]

let%expect_test _ =
  let e = parse [
    "(define positive? (lambda (d) (> d 0)))";
    "(if (positive? -1) 'mario 'luigi)"
  ] in
  eval_and_print_prog e;
  [%expect {| 'luigi |}]

let%expect_test _ =
  let e = parse ["(let ((x 1) (y 2)) (+ x y))"] in
  eval_and_print_prog e;
  [%expect {| 3 |}]

let%expect_test _ =
  let e = parse ["(let ((sum2 (lambda (x y) (+ x y)))) (sum2 100 200))"] in
  eval_and_print_prog e;
  [%expect {| 300 |}]

let%expect_test _ =
  let e = parse ["(let ((x 1) (y x)) x)"] in
  eval_and_print_prog e;
  [%expect {| 1 |}]

let%expect_test _ =
  let e = parse ["(let ((x 1) (y (+ x 1)) (z (+ x y))) (+ x y z))"] in
  eval_and_print_prog e;
  [%expect {| 6 |}]

let%expect_test _ =
  let e = parse [
   "(let (";
   "  (x (let ((x 1) (y 2)) (+ x y)))";
   "  (y 3)";
   ") (- x y))";
  ] in
  eval_and_print_prog e;
  [%expect {| 0 |}]

let%expect_test _ =
  let e = parse [
    "(define incr (lambda (x) (+ x 1)))";
    "(define decr (lambda (x) (- x 1)))";
    "((if false incr decr) 0)"
  ] in
  eval_and_print_prog e;
  [%expect {| -1 |}]

let%expect_test _ =
  let e = parse [
    "(define incr (lambda (x) (+ x 1)))";
    "(define decr (lambda (x) (- x 1)))";
    "((if true incr decr) 0)"
  ] in
  eval_and_print_prog e;
  [%expect {| 1 |}]

let%expect_test _ =
  let e = parse [
    "(define empty? (lambda (l) (= l '())))";
    "(empty? '())"
  ] in
  eval_and_print_prog e;
  [%expect {| true |}]
  
let%expect_test _ =
  let e = parse [
    "(define empty? (lambda (l) (= l '())))";
    "(empty? '(0 1 2 3 4 5))"
  ] in
  eval_and_print_prog e;
   [%expect {| false |}]

let%expect_test _ =
   let e = parse [
     "(define f (lambda () 3))";
     "(f)"
   ] in
   eval_and_print_prog e;
  [%expect {| 3 |}]

let%expect_test _ =
  let e = parse [
    "(define empty? (lambda (l) (= l '())))";
    "(let-rec ((x 3)) x)"
  ] in
  eval_and_print_prog e;
  [%expect {| 3 |}]

let%expect_test _ =
  let e = parse [
    "(define empty? (lambda (l) (= l '())))";
    "(define incr (lambda (x) (+ x 1)))";
    "(let-rec ((map (lambda (f l)   ";
    "            (if                ";
    "              (empty? l)       ";
    "              '()              ";
    "              (cons (f (car l)) (map f (cdr l)))";
    "            ))))               ";
    "            (map incr '()))    ";
  ] in
  eval_and_print_prog e;
  [%expect {| '() |}]

let%expect_test _ =
  let e = parse [
    "(define empty? (lambda (l) (= l '())))";
    "(define incr (lambda (x) (+ x 1)))";
    "(let-rec ((map (lambda (f l)   ";
    "            (if                ";
    "              (empty? l)       ";
    "              '()              ";
    "              (cons (f (car l)) (map f (cdr l)))";
    "            ))))               ";
    "            (map incr '(0 1 2 3 -1)))";
  ] in
  eval_and_print_prog e;
  [%expect {| '(1 2 3 4 0) |}]

let%expect_test _ =
  let e = parse [
    "(define empty? (lambda (l) (= l '())))";
    "(define-rec length (lambda (l)";
    "  (if (empty? l)";
    "      0";
    "      (+ 1 (length (cdr l)))"; 
    "  )))";
    "(length '(100 200 400 '(700 800)))"
  ] in
  eval_and_print_prog e;
  [%expect {| 4 |}]

let%expect_test _ =
  let e = parse [
     "(define-rec fold (lambda (f i l)";
     "  (if (= l '())";
     "      i";
     "      (fold f (f i (car l)) (cdr l))";
     "  )))";
     "(define sum (lambda (l) (fold (lambda (x y) (+ x y)) 0 l)))";
     "(sum '(1000 -100 33 66))"
  ] in
  eval_and_print_prog e;
  [%expect {| 999 |}]
