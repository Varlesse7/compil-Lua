open Ast
open Compiler

let tl = function
  | _ :: t -> t
  | [] -> failwith "tl: empty list"




let rec eval_com (com : com) (s : value list) : (value list) =
  match com with
    | Quote (x) -> [x]@(tl s)
    
    | Car -> (
        match s with
        | Pair (v1, v2) :: tail -> 
            Printf.printf "1ere = %s\n" (string_of_value v1); 
            Printf.printf "2eme = %s\n" (string_of_value v2);
            v1 :: tail
        | _ -> failwith "Car applied to non-pair"
      )

    | Cdr -> (
        match s with
        | Pair (v1, v2) :: tail -> Printf.printf "1ere = %s\n" (string_of_value v1); 
        Printf.printf "2eme = %s\n" (string_of_value v2);
        v2 :: tail
        | Closure (_, v) :: tail -> Printf.printf "2eme = %s\n" (string_of_value v);
        v :: tail
        | _ -> failwith ""
      )

    | Cons -> (
        match s with
        | v2 :: v1 :: tail -> Pair (v1, v2) :: tail
        | _ -> failwith "Stack underflow for Cons"
      )

    | Push -> (
          match s with
          | v :: tail -> v :: v :: tail
          | _ -> failwith "Stack underflow for Push"
        )

    | Swap -> (
        match s with
        | v1 :: v2 :: tail -> v2 :: v1 :: tail
        | _ -> failwith "Stack underflow for Swap"
      )

    | Op op -> (
        match s with
        | Pair (v1, v2) :: rest -> (
            match op with
            | Add -> (match v1, v2 with
                | Int x, Int y -> Int (x + y) :: rest
                | _ -> failwith "Op Add sur types invalides")
            | Sub -> (match v1, v2 with
                | Int x, Int y -> Int (x - y) :: rest
                | _ -> failwith "Op Sub sur types invalides")
            | Mult -> (match v1, v2 with
                | Int x, Int y -> Int (x * y) :: rest
                | _ -> failwith "Op Mult sur types invalides")
            | Eq -> (match v1, v2 with
                | Int x, Int y ->
                  Printf.printf "\n%s\n" (string_of_value v2) ;
                  if x == y then (Bool true) :: rest else (Bool false) :: rest
                | _ -> failwith "Op Add sur types invalides")
          )
        | _ -> failwith "Op nécessite au moins deux éléments sur la pile"
      )
    
    | Branch (c1, c2) -> (
      match s with
      | Bool b :: rest -> 
          if b then eval_coms c1 rest
          else eval_coms c2 rest
      | _ -> failwith "Branch nécessite un booléen au sommet de la pile"
    )

    | Cur c -> (match s with
        | [] -> failwith "Pas d'arguments"
        | x::xs -> Closure (c, x) :: xs)
    
    | App -> (
        match s with
        | Pair (Closure (code_body, env), v) :: tail -> 
            (* Affiche la valeur passée en argument *)
            Printf.printf "v = %s\n" (string_of_value v);

            (* Étend l’environnement en construisant une paire : (env, v) *)
            let extended_env = Pair (env, v) in

            (* Exécute le code du closure avec la pile initialisée à [extended_env] *)
            let res_stack = eval_coms code_body [extended_env] in

            (* Résultat en tête de pile *)
            (match res_stack with
              | res :: _ -> res :: tail
              | _ -> failwith "Le code du closure n'a pas produit de résultat")
        | c :: _-> Printf.printf "Error App on :%s"(string_of_value c); failwith ""
        | _ -> failwith ""
        )


    
    | Rplac -> (
      match s with
      | v :: env :: tail ->
          Printf.printf " (1) Rplac on :%s \n" (string_of_value env);
          let replaced = rplac_subst NullValue env v in
          Printf.printf " (2) Rplac on :%s \n" (string_of_value replaced);
          Pair(replaced, env) :: tail
      | _ -> failwith "Rplac requires two values on the stack"
    )

and rplac_subst dummy replacement = function
  | NullValue -> 
    if dummy = NullValue 
      then (
        replacement )
      else 
        NullValue
  | Int _ | Bool _ as v -> 
    v
  | Pair (v1, v2) -> 
    Pair (rplac_subst dummy replacement v1, rplac_subst dummy replacement v2)
  | Closure (c, env) -> 
    Closure (c, rplac_subst dummy replacement env)
  

and eval_coms (coms : coms) (stack : value list) : value list =
  match coms with
  | [] -> stack
  | c :: cs -> 
      let stack' = eval_com c stack in
      eval_coms cs stack'




