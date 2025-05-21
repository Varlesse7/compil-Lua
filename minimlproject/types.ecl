(* types.ecl *)
type value = 
  Int of int 
  | Bool of bool 
  | NullValue
  | Pair of int * int (* chaque int représente la position d'une value dans la stack *)
  | Closure of instruction array * int;; (* même chose que pour pair *)

type instruction =
  | Push 
  | Quote of value 
  | Cons 
  | Car 
  | Cdr 
  | Swap
  | Op of op 
  | App 
  | Cur of int
  | Rplac 
  | Branch of int * int ;;

type op = Add | Sub | Mult | Eq ;;
