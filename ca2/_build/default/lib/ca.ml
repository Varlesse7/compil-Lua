open Mltocam

let tl = function
  | _ :: t -> t
  | [] -> failwith "tl: empty list"


let rec eval_com (com : com) (s : value list) : (value list) =
  match com with
    | Quote (x) -> [x]@(tl s)
    
    | Car -> (
        match s with
        | Pair (v1, v2) :: tail -> (Printf.printf "1ere = %s\n" (string_of_value v1); 
        Printf.printf "2eme = %s\n" (string_of_value v2);
        v1 :: tail)
        | _ -> failwith "Car applied to non-pair"
      )

    | Cdr -> (
        match s with
        | Pair (v1, v2) :: tail -> Printf.printf "1ere = %s\n" (string_of_value v1); 
        Printf.printf "2eme = %s\n" (string_of_value v2);
        v2 :: tail
        | _ -> failwith "Cdr applied to non-pair"
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
          (* On exécute le code du closure avec l’environnement étendu par la valeur v *)
          Printf.printf "v = %s\n" (string_of_value v);
          let new_stack = Pair(NullValue, v) :: [env] in
          let res_stack = eval_coms code_body new_stack in
          
          (* Après exécution, on remplace le closure et son argument par le résultat au sommet *)
          (match res_stack with
            | res :: _ -> res :: tail
            | _ -> failwith "Closure code did not produce result")
      | _ -> failwith "Stack underflow for App"
    )

    
    | Rplac -> (
        match s with
        | v' :: Pair (v, _) :: tail ->
            Pair (v, v') :: tail
        | _ -> failwith "Rplac requires a value and a pair"
    )


and eval_coms (coms : coms) (stack : value list) : value list =
  match coms with
  | [] -> stack
  | c :: cs -> 
      let stack' = eval_com c stack in
      eval_coms cs stack'




