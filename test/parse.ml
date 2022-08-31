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
  let e = Option.get (parse_string (String.concat "\n" [
    "(define pi (* 2 (acos 0.)))";
    "(define deg->rad (lambda (d) (* d (/ pi 180))))"
  ])) in
  printf "%s" (pretty_prog e);
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
