open Rascal.Ast
open Rascal.Parse
open Printf

let parse_and_print lines =
  try
    let e = Option.get (parse_string (String.concat "\n" lines)) in
    printf "%s" (pretty_prog e)
  with
  | _ -> ()

(* Programs that should parse: *)

let%expect_test _ =
  parse_and_print [];
  [%expect {||}]

let%expect_test _ =
  parse_and_print ["()"];
  [%expect {| () |}]

let%expect_test _ =
  parse_and_print ["(+)"];
  [%expect {| (+) |}]

let%expect_test _ =
  parse_and_print ["(+ 1 2)"];
  [%expect {| (+ 1 2) |}]

let%expect_test _ =
  parse_and_print [
    "(define pi (* 2 (acos 0.)))";
    "(define deg->rad (lambda (d) (* d (/ pi 180))))"
  ];
  [%expect {|
    (define pi (* 2 (acos 0.)))
    (define deg->rad (lambda (d) (* d (/ pi 180))))
  |}]

let%expect_test _ =
  parse_and_print ["(()())"];
  [%expect {| (() ()) |}]

let%expect_test _ =
  parse_and_print ["((()))"];
  [%expect {| ((())) |}]

let%expect_test _ =
  parse_and_print ["'+"];
  [%expect {| '+ |}]

let%expect_test _ =
  parse_and_print ["'()"];
  [%expect {| '() |}]

let%expect_test _ =
  parse_and_print ["'('+)"];
  [%expect {| '('+) |}]

let%expect_test _ =
  parse_and_print ["'(1 2 3 4)"];
  [%expect {| '(1 2 3 4) |}]

let%expect_test _ =
  parse_and_print ["'('(1 2 3) '(4 5) '(6 '(7 8)))"];
  [%expect {| '('(1 2 3) '(4 5) '(6 '(7 8))) |}]

let%expect_test _ =
  parse_and_print [
    "(define positive? (lambda (d) (> d 0)))";
    "(if (postive? 1) 'mario 'luigi)"];
  [%expect {|
    (define positive? (lambda (d) (> d 0)))
    (if (postive? 1) 'mario 'luigi) |}]

let%expect_test _ =
  parse_and_print ["(let ((x 1) (y 2)) (+ x y))"];
  [%expect {| (let ((x 1) (y 2)) (+ x y)) |}]

let%expect_test _ =
  parse_and_print ["(let ((sum2 (lambda (x y) (+ x y)))) (sum2 100 200))"];
  [%expect {| (let ((sum2 (lambda (x y) (+ x y)))) (sum2 100 200)) |}]

let%expect_test _ =
  parse_and_print ["(let ((x 1) (y x)) x)"];
  [%expect {| (let ((x 1) (y x)) x) |}]

let%expect_test _ =
  parse_and_print ["(let ((x 1) (y (+ x 1)) (z (+ x y))) (+ x y z))"];
  [%expect {| (let ((x 1) (y (+ x 1)) (z (+ x y))) (+ x y z)) |}]

(* Programs that should not parse: *)

let%expect_test _ =
  parse_and_print ["(')"];
  [%expect {| (string):1:3: syntax error |}]

let%expect_test _ =
  parse_and_print ["("];
  [%expect {| (string):1:1: syntax error |}]

let%expect_test _ =
  parse_and_print ["())"];
  [%expect {| (string):1:3: syntax error |}]
