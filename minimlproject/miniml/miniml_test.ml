open Compiler
open Cam_interpreter
open Flatten

let () =
  let fname = Sys.argv.(1) in
  let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
    print_expr p;
    let code_cam = compile_program p in
    let resultat = eval_coms code_cam [NullValue] in
    let flat_code = flatten code_cam in

    print_string "\nCode CAM généré \n";
    print_code_cam code_cam;
    print_string ".\n";

    ( match resultat with
      | Int n :: _ -> Printf.printf "Résultat : %d\n" n
      | Bool b :: _ -> Printf.printf "Résultat : %b\n" b
      | NullValue :: _ -> Printf.printf "Null\n"
      | _ -> Printf.printf "Résultat inattendu\n");

    Printf.printf "Code CAM plat :\n";
    Array.iteri (fun i instr ->
      Printf.printf "%2d: %s\n" i (string_of_flat instr)
    ) flat_code;
    Printf.printf "\nCode Éclat généré :\n";
    generate_eclat_to_file "code.ecl" flat_code


  with Lexer.Eof ->
    exit 0
