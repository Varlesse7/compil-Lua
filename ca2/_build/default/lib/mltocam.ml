module StringMap = Map.Make(String)

type ident = string

type pat =
  | Pairpat of pat * pat
  | IdentPat of ident
  | NullPat

type expr =
  | Ident of ident
  | Number of int
  | False
  | True
  | Apply of expr * expr
  | Mlpair of expr * expr
  | Lambda of pat * expr
  | Let of pat * expr * expr
  | LetRec of pat * expr * expr
  | If of expr * expr * expr


type program = coms
and coms = com list
and com =
  | Quote of value
  | Op of operator
  | Car
  | Cdr
  | Cons
  | Push
  | Swap
  | App
  | Rplac
  | Cur of coms
  | Branch of coms * coms

and value =
  | Int of int
  | Bool of bool
  | Quote of pat 
  | Pair of value * value
  | Closure of coms * value 
  | NullValue

and operator = Add | Sub | Mult | Eq


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
  | IdentPat x -> if x == i then Some [] else None
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
  compile (IdentPat "null") e


let string_of_operator = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mult -> "Mult"
  | Eq -> "Eq"

let rec string_of_value = function
  | Int n -> Printf.sprintf "Int(%d)" n
  | Bool b -> Printf.sprintf "Bool(%b)" b
  | Quote _ -> "Quote(envMap)"  (* simplifié, à adapter si tu veux plus de détails *)
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
