open Rascal.Ast
open Rascal.Context
open Rascal.Eval
open Rascal.Parse
open Printf

module Eval = Rascal.Eval

let batch ~print_result files =
  let eval_file (context, _) file =
    match parse_file file with
    | None -> exit 1
    | Some program ->
      try match eval_prog context program with
      | None -> context, None
      | Some (context', e) -> context', Some e
      with
      | Runtime_error msg -> fprintf stderr "Runtime error: %s\n" msg; exit 1
  in
  let (context, result) = List.fold_left eval_file (empty_context, None) files in
  begin match print_result, result with
  | true, Some e -> printf "%s\n" (pretty_expr e)
  | _ -> ()
  end;
  context

let repl init_files =
  let prompt = "rascal $ " in
  let rec loop context = 
    printf "%s" prompt; flush stdout;
    let input = read_line () in
    (* user commands *)
    begin match Str.split (Str.regexp "[ \t]+") input with
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
        flush stderr;
        loop context
    end
  in
  let context =
    match init_files with
    | [] -> empty_context
    | _ -> batch ~print_result:false init_files
  in
  try loop context with
  | End_of_file -> printf "Exiting...\n"; flush stdout

let main () = 
  let push r x = r := x :: !r in
  let init_files = ref [] in
  let batch_files = ref [] in
  let spec_list = [
    ("--init", 
     Arg.String (push init_files),
     "Load the interpreter with the given file. May be specified multiple times.")
  ] in
  let usage_msg = "rascal [--init <source1> [--init <source2> ...] | <source1> [<source2> ...]]" in
  Arg.parse spec_list (push batch_files) usage_msg;
  batch_files := List.rev (!batch_files);
  init_files := List.rev (!init_files);
  match List.length (!init_files) > 0, List.length (!batch_files) > 0 with
  | true, true -> Arg.usage spec_list usage_msg
  | false, false -> repl []
  | true, false -> repl !init_files
  | false, true -> ignore (batch ~print_result:true (!batch_files))

let () = main ()
