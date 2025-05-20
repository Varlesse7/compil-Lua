(* main.ml *)

let () =
  let file = Sys.argv.(1) in
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  let ast = Miniml.Parser.program Miniml.Lexer.token lexbuf in
  close_in ic;

  (* Compilation en code CAM *)
  let cam_code = Miniml.Compiler.compile ast in

  (* Interprétation directe en OCaml *)
  let result_ocaml = CamInterpreter.run cam_code in
  Printf.printf "Résultat via interpréteur OCaml : %d\n" result_ocaml;

  (* Aplatissement CAM et écriture fichier .eclat *)
  let flat_code = Eclat.FlattenCam.flatten cam_code in
  let eclat_file = Filename.remove_extension file ^ ".eclat" in
  Eclat.FlattenCam.write_to_file flat_code eclat_file;

  (* Interprétation en Eclat *)
  let result_eclat = Eclat.EclatInterpreter.run_file eclat_file in
  Printf.printf "Résultat via interpréteur Eclat : %d\n" result_eclat
