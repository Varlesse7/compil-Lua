open Ast

module StringMap = Map.Make(String)


(* Fonction de parsing des opérateurs binaires *)
let operation (x : string) : operator =
  match x with
  | "add" -> Add
  | "sub" -> Sub
  | "mult" -> Mult
  | "eq" -> Eq
  | _ -> failwith "Opérateur non supporté"



(* Génère le code CAM pour accéder à la variable en suivant son chemin *)
let rec access_code (i:ident) (env : pat) : coms option =
  match env with
  | Pairpat (p1, p2) -> (match access_code i p1 with
                        | Some c -> Some (Car :: c)
                        | None -> (match access_code i p2 with
                                  | Some c -> Some (Cdr :: c)
                                  | None -> None))
  | IdentPat x -> if x = i then Some [] else None
  | NullPat -> None

(* Compilation d'une expression vers des instructions CAM *)
let rec compile (env : pat) (e : expr) : coms =
  match e with
  | Number n -> [Quote (Int n)]
  | True -> [Quote (Bool true)]
  | False -> [Quote (Bool false)]
  | Ident i -> (match access_code i env with
            | Some liste -> liste
            | None -> failwith "Variable introuvable")
  | If (e1, e2, e3) ->
      [Push] @ (compile env e1) @ [Branch (compile env e2, compile env e3)]
  | Mlpair (e1, e2) ->
      [Push] @ (compile env e1) @ [Swap] @ compile env e2 @ [Cons]
  | Let (p, e1, e2) ->
      let code_e1 = compile env e1 in
      let new_env = Pairpat (env , p) in
      [Push] @ code_e1 @ [Cons] @ compile new_env e2
  | LetRec (p, e1, e2) ->
      let new_env = Pairpat (env , p) in
      let code_e1 = compile new_env e1 in
      let code_e2 = compile new_env e2 in
      [Push; Quote NullValue; Cons; Push] @ code_e1 @ [Swap; Rplac] @ code_e2
  | Lambda (p, body) ->
      let new_env = Pairpat (env, p) in
      [Cur (compile new_env body)]
  | Apply (Ident "fst", e2) -> compile env e2 @ [Car]
  | Apply (Ident "snd", e2) -> compile env e2 @ [Cdr]
  | Apply (Ident x, e2) when List.mem x ["add"; "sub"; "mult"; "eq"] ->
      compile env e2 @ [Op (operation x)]
  | Apply (e1, e2) ->
      [Push] @ compile env e1 @ [Swap] @ compile env e2 @ [Cons; App]


let compile_program (e : expr) : coms =
  compile NullPat e

let rec print_pat p =
  match p with
  | NullPat -> "NullPat"
  | IdentPat id -> Printf.sprintf "IdentPat(%s)" id
  | Pairpat (p1, p2) ->
      Printf.sprintf "Pairpat(%s, %s)" (print_pat p1) (print_pat p2)

let rec print_expr ?(indent=0) e =
  let indent_str = String.make indent ' ' in
  match e with
  | Ident id ->
      Printf.printf "%sIdent(%s)\n" indent_str id
  | Number n ->
      Printf.printf "%sNumber(%d)\n" indent_str n
  | False ->
      Printf.printf "%sFalse\n" indent_str
  | True ->
      Printf.printf "%sTrue\n" indent_str
  | Apply (e1, e2) ->
      Printf.printf "%sApply(\n" indent_str;
      print_expr ~indent:(indent + 2) e1;
      print_expr ~indent:(indent + 2) e2;
      Printf.printf "%s)\n" indent_str
  | Mlpair (e1, e2) ->
      Printf.printf "%sMlpair(\n" indent_str;
      print_expr ~indent:(indent + 2) e1;
      print_expr ~indent:(indent + 2) e2;
      Printf.printf "%s)\n" indent_str
  | Lambda (p, e) ->
      Printf.printf "%sLambda(%s,\n" indent_str (print_pat p);
      print_expr ~indent:(indent + 2) e;
      Printf.printf "%s)\n" indent_str
  | Let (p, e1, e2) ->
      Printf.printf "%sLet(%s,\n" indent_str (print_pat p);
      print_expr ~indent:(indent + 2) e1;
      print_expr ~indent:(indent + 2) e2;
      Printf.printf "%s)\n" indent_str
  | LetRec (p, e1, e2) ->
      Printf.printf "%sLetRec(%s,\n" indent_str (print_pat p);
      print_expr ~indent:(indent + 2) e1;
      print_expr ~indent:(indent + 2) e2;
      Printf.printf "%s)\n" indent_str
  | If (e1, e2, e3) ->
      Printf.printf "%sIf(\n" indent_str;
      print_expr ~indent:(indent + 2) e1;
      print_expr ~indent:(indent + 2) e2;
      print_expr ~indent:(indent + 2) e3;
      Printf.printf "%s)\n" indent_str


let string_of_operator = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mult -> "Mult"
  | Eq -> "Eq"

let rec string_of_value = function
  | Int n -> Printf.sprintf "Int(%d)" n
  | Bool b -> Printf.sprintf "Bool(%b)" b
  | Pair (v1, v2) ->
      Printf.sprintf "Pair(%s, %s)" (string_of_value v1) (string_of_value v2)
  | Closure (_, _) -> "Closure(<code>, <env>)"
  | NullValue -> "NullValue"

let rec string_of_com = function
  | Quote v -> "Quote(" ^ string_of_value v ^ ")"
  | Op op -> "Op(" ^ string_of_operator op ^ ")"
  | Car -> "Car"
  | Cdr -> "Cdr"
  | Cons -> "Cons"
  | Push -> "Push"
  | Swap -> "Swap"
  | App -> "App"
  | Rplac -> "Rplac"
  | Cur coms ->
      let cs = String.concat "; " (List.map string_of_com coms) in
      "Cur([" ^ cs ^ "])"
  | Branch (c1, c2) ->
      let s1 = String.concat "; " (List.map string_of_com c1) in
      let s2 = String.concat "; " (List.map string_of_com c2) in
      "Branch([" ^ s1 ^ "], [" ^ s2 ^ "])"

let print_code_cam (prog : com list) =
  List.iter (fun c -> print_endline (string_of_com c)) prog



