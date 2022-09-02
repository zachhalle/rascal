open Rascal.Ast
open Rascal.Context
open Rascal.Eval
open Rascal.Parse
open Printf

module Eval = Rascal.Eval

let repl () =
  let prompt = "rascal $ " in
  let rec loop context = 
    printf "%s" prompt; flush stdout;
    let input = read_line () in
    (* user commands *)
    begin match List.filter ((<>) "") (String.split_on_char ' ' input) with
    | ":context" :: vs ->
      let maybe_print v =
        begin match substitute_opt context v with
        | None -> printf "%s is undefined\n" v
        | Some e -> printf "%s is bound to %s\n" v (pretty_expr e)
        end
      in
      List.iter maybe_print vs;
      loop context
    | _ -> ()
    end;
    (* read, eval, print *)
    begin match parse_string input with
    | None -> loop context
    | Some e ->
      try match eval_prog context e with
      | None -> loop context
      | Some (context', e') ->
        printf "%s\n" (pretty_expr e');
        loop context'
      with
      | Eval.Runtime_error msg ->
        fprintf stderr "%s\n" msg;
        loop context
    end
  in
  try loop empty_context with
  | End_of_file -> printf "Exiting..."; flush stdout

let () = repl ()
