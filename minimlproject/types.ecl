(* types.ecl *)
type value = Int of int | Bool of bool | NullValue
           | Pair of value * value
           | Closure of instruction array * value ;;

type instruction =
  | Push | Quote of value | Cons | Car | Cdr | Swap
  | Op of op | App | Cur of int
  | Rplac | Branch of int * int ;;

type op = Add | Sub | Mult | Eq ;;
