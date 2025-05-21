open Compiler
open Cam_interpreter
open Flatten

let () =
  let fname = Sys.argv.(1) in
  let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
    let code_cam = compile_program p in
    print_string "\nCode CAM généré \n";
    print_code_cam code_cam;
    print_string ".\n";

    let resultat = eval_coms code_cam [ Pair(NullValue, Closure([Cdr;Op Add], NullValue)); Closure([Cdr;Op Sub], NullValue) ] in

    let flat_code = flatten code_cam in

    ( match resultat with
      | Int n :: _ -> Printf.printf "Résultat : %d\n" n
      | Bool b :: _ -> Printf.printf "Résultat : %b\n" b
      | NullValue :: _ -> Printf.printf "Null\n"
      | _ -> Printf.printf "Résultat inattendu\n");

    Printf.printf "\nCode CAM plat écrit\n";
    Printf.printf "\nCode Éclat généré dans le fichier code.ecl\n";
    generate_eclat_to_file "code.ecl" flat_code


  with Lexer.Eof ->
    exit 0
