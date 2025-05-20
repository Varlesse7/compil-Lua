open Ca (* ou ton module CAM *)
open Mltocam  (* ou ton module CAM *)

module StringMap = Map.Make(String)

(* Exemple d'AST MiniML *)
(* let exemple_expr = Apply (Lambda (Pairpat (IdentPat "x", IdentPat "y") , Apply (Ident "add", Mlpair (Ident "x", Ident "y"))), Mlpair (Number 42, Number 43)) *)
(* let exemple_expr = Let (Pairpat (IdentPat "x", IdentPat "y"), Mlpair (Number 42, Number 43), Ident "y") *)

(* let exemple_expr = Let (IdentPat "x", Number 43, Let(IdentPat "y", Number 44, Let (IdentPat "z", Number 6, Apply (Ident "add", Mlpair (Ident "x", Ident "y")) ))) *)

(* let exemple_expr =
  Let (Pairpat (IdentPat "a", Pairpat (IdentPat "b", IdentPat "c")),
    Mlpair (Number 1, Mlpair (Number 2, Number 3)),
    Apply (Ident "add", Mlpair (Ident "a", Ident "c"))) *)

let exemple_expr =
  Let(IdentPat "x", Number 42, Ident "x")

(* let exemple_expr =Let(IdentPat "x", Number 3,  If (Apply (Ident "eq", Mlpair (Ident "x", Number 0)), Number 0, Number 2) ) *)

(* let exemple_expr =
  LetRec (
    IdentPat "x", Apply(Ident "add", Mlpair(Number 3,Apply(Ident "add", Mlpair(Number 5, Number 3)))), Ident "x"
  ) *)


(* Compilation *)
let code_cam = compile_program (Let(IdentPat "x", Number 42, Ident "x"))

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
