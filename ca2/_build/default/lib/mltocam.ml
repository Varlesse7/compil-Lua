module StringMap = Map.Make(String)
type envMap = int StringMap.t  (* ident -> position *)

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
  | Quote of envMap 
  | Pair of value * value
  | Closure of coms * value 
  | NullValue

and operator = Add | Sub | Mult

(* Fonction de parsing des opérateurs binaires *)
let operation (x : string) : operator =
  match x with
  | "add" -> Add
  | "sub" -> Sub
  | "mult" -> Mult
  | _ -> failwith "Opérateur non supporté"

(* Fonction pour extraire tous les identifiants d’un pattern *)
let rec pat_idents p =
  match p with
  | IdentPat id -> [id]
  | Pairpat (p1, p2) -> pat_idents p1 @ pat_idents p2
  | NullPat -> []

(* Associe à chaque identifiant une position dans l’environnement *)
let assign_positions ids base =
  List.mapi (fun i id -> (id, i + base)) ids

(* Ajoute tous les identifiants d’un pattern à l’environnement *)
let extend_env (env : envMap) (p : pat) : envMap =
  let ids = pat_idents p in
  let new_bindings = assign_positions ids 0 in
  let shifted_env = StringMap.map (fun pos -> pos + List.length ids) env in
  List.fold_left (fun acc (id, pos) -> StringMap.add id pos acc) shifted_env new_bindings


(* Code CAM pour accéder à la variable à la position donnée *)
let rec access_code (n : int) : coms =
  if n = 0 then [Car]
  else Cdr :: access_code (n - 1)

(* Compilation d'une expression vers des instructions CAM *)
let rec compile (env : envMap) (e : expr) : coms =
  match e with
  | Number n -> [Quote (Int n)]
  | True -> [Quote (Bool true)]
  | False -> [Quote (Bool false)]
  | Ident i ->
      begin match StringMap.find_opt i env with
      | Some pos -> access_code pos
      | None -> failwith ("Variable libre ou non liée : " ^ i)
      end
  | If (e1, e2, e3) ->
      [Push] @ (compile env e1) @ [Branch (compile env e2, compile env e3)]
  | Mlpair (e1, e2) ->
      [Push] @ (compile env e1) @ [Swap] @ compile env e2 @ [Cons]
  | Let (p, e1, e2) ->
      let code_e1 = compile env e1 in
      let new_env = extend_env env p in
      [Push] @ code_e1 @ [Cons] @ compile new_env e2
  | LetRec (p, e1, e2) ->
      let new_env = extend_env env p in
      let code_e1 = compile new_env e1 in
      let code_e2 = compile new_env e2 in
      [Push; Quote NullValue; Cons; Push] @ code_e1 @ [Swap; Rplac] @ code_e2
  | Lambda (p, body) ->
      let new_env = extend_env env p in
      [Cur (compile new_env body)]
  | Apply (Ident "fst", e2) -> compile env e2 @ [Car]
  | Apply (Ident "snd", e2) -> compile env e2 @ [Cdr]
  | Apply (Ident x, e2) when List.mem x ["add"; "sub"; "mult"] ->
      compile env e2 @ [Op (operation x)]
  | Apply (e1, e2) ->
      [Push] @ compile env e1 @ [Swap] @ compile env e2 @ [Cons; App]


let compile_program (e : expr) : coms =
  compile StringMap.empty e


let string_of_operator = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mult -> "Mult"

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
