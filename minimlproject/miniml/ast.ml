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
  | Pair of value * value
  | Closure of coms * value 
  | NullValue

and operator = Add | Sub | Mult | Eq

