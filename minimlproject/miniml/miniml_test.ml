open Compiler

let () =
  let fname = Sys.argv.(1) in
  let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
    print_expr p;
    let code = compile_program p in
    print_code_cam code;
    print_string ".\n"
  with Lexer.Eof ->
    exit 0
