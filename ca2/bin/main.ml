module StringMap = Map.Make(String);;

type ident = string
type envMap = (StringMap.t) 

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
| Env of envM ap
| Bool of bool
| NullValue
and operator = Add | Sub | Mult

let compile (env: Map.Make String) (e:expr) : coms =
  match e with
  | Number n -> [Quote (Int n)]
  | True -> [Quote (Bool true)] 
  | False -> [Quote (Bool false)] 
  | Ident i -> 
    match env.find_opt i env with 
      Some n -> compile env n 
    | Nothing -> assert false
  | If e1 e2 e3 -> Push@(compile env e1)@Branch (compile env e2) (compile env e3) 
  | Mlpair e1 e2 -> Push@(compile env e1)@ Swap@(compile env e2)@Cons
  | Let p e1 e2 -> Push@(compile env e1)@Cons@(compile (p::env) e2)
  | LetRec p e1 e2 -> Push@Quote(env)@Cons@Push@(compile env e1)@Swap@Rplac@(compile (p::env) e2)
  | Lambda p e -> Cur (compile env e)
  | Apply (Ident "fst") e2 -> compiler(env e2) @ Car
  | Apply (Ident "snd") e2 -> compiler(env e2) @ Cdr
  | Apply (Ident x) e2 -> compiler(env e2) @ (Op (x)) 
  | Apply e1 e2 -> Push@(compile env e1) @ Swap @ (compile env e2) @ Cons@ App


