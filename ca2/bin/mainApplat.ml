module StringMap = Map.Make(String);;

type ident = string
type envMap = (StringMap.t) 

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

and comflat = (*Changer le value et l'operator pour ne plus dépendre d'élément de CAM*)
  | Quotef of value
  | Opf of operator
  | Carf
  | Cdrf
  | Consf
  | Pushf 
  | Swapf
  | Appf
  | Rplacf
  | Curf of int 
  | Branchf of int * int
  | NullCom


let loop (pos: int) (cs: coms) : int = 
  match cs with 
    [] -> pos
  | (c::cs) -> 
    match c with 
      Quote x -> (
        let pos = pos+1 in
          loop pos cs
      ) 
    | Op x -> (
      let pos = pos+1 in  
        loop pos cs
    )
    | Car -> (
      let pos = pos + 1 in 
        loop pos cs 
    )
    | Cdr -> (
      let pos = pos + 1 in 
        loop pos cs 
    )
    | Push -> (
      let pos = pos + 1 in 
        loop pos cs 
    ) 
    | Swap -> (
      let pos = pos + 1 in 
        loop pos cs 
    ) 
    | App -> (
      let pos = pos + 1 in 
        loop pos cs 
    ) 
    | Rplac -> (
      let pos = pos + 1 in 
        loop pos cs 
    ) 
    | Cur css -> (
      let pos = loop pos css in 
        loop pos cs 
    )
    | Branch cs1 cs2 -> (
      let pos1 = loop pos cs1 in 
        let pos2 = loop pos1 cs2 in 
          loop pos2 cs
    )

let compile (bytearray:comflat array) (c:com) (pos:int) : (comflat array) = (*Modifier pour parcourir une liste de coms plutot qu'un com*)
  match c with
  | Quote x -> bytearray.(pos) <- Quotef x
  | Op op -> bytearray.(pos) <- Opf op 
  | Car -> bytearray.(pos) <- Carf  (*Manipule implicitement le pos+1*)
  | Cdr -> bytearray.(pos) <- Cdrf
  | Cons -> bytearray.(pos) <- Consf 
  | Push -> bytearray.(pos) <- Pushf
  | Swap -> bytearray.(pos) <- Swapf
  | Appf -> bytearray.(pos) <- Appf
  | Rplac -> bytearray.(pos) <- Rplacf
  | Cur env -> bytearray.(pos) <- Curf (loop env pos) (*utilise implicitement pos+1*)
  | Branch cs1 cs2 -> bytearray.(pos) <- 
    let pos1 =  (loop cs1 pos) in 
      let pos2 =  (loop cs2 pos) in 
        Branchf pos1 pos2

