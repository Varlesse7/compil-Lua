open Ca (* ou ton module CAM *)
open Mltocam  (* ou ton module CAM *)

module StringMap = Map.Make(String)

(* Exemple d'AST MiniML *)
(* let exemple_expr = Apply (Lambda (Pairpat (IdentPat "x", IdentPat "y") , Apply (Ident "add", Mlpair (Ident "y", Number 42))), Mlpair (Number 42, Number 43)) *)
let exemple_expr = Let (Pairpat (IdentPat "x", IdentPat "y"), Mlpair (Number 42, Number 43), Ident "x")
(* Compilation *)
let code_cam = compile StringMap.empty exemple_expr

(* Affichage du code CAM généré *)
let () =
  print_endline "Code CAM généré :";
  print_code_cam code_cam


(* Exécution *)
let resultat = eval_coms code_cam [NullValue]

(* Affichage du résultat *)
let () =
  match resultat with
  | Int n :: _ -> Printf.printf "Résultat : %d\n" n
  | Bool b :: _ -> Printf.printf "Résultat : %b\n" b
  | NullValue :: _ -> Printf.printf "Null\n"
  | _ -> Printf.printf "Résultat inattendu\n"
