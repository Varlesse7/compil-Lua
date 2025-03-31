open cpa

module StringMap = Map.Make(String);;
module List;;

let eval_com com s =
  match com with
      Quote (Int x) -> [x]@(tl s)
    | Quote (Bool x) -> [x]@(tl s)
    | Quote (x) -> [x]@(tl s)
    | Car -> (
      match s with 
          [] -> assert false
        | [(h, t)] -> [h]
        | ((h, t)::s2) -> [h]@s2
    )
    | Cdr -> (
      match s with 
          [] -> assert false
        | [(h, t)] -> [t]
        | ((h, t)::s2) -> [t]@s2
    )
    | Cons ->(
      match s with 
        [] -> assert false
      | [h] -> assert false
      | (h::t::s2) -> [(t, h)]::s2
    )
    | Push -> (
      match s with 
        [] -> assert false
      | (x::s2) -> (x::x::s2)  
    )
    | Swap -> (
      match s with 
        [] -> assert false
      | [a] -> assert false
      | (a::b::s2) -> (b::a::s2)
    )
    | Op n -> (
      match s with 
        [] -> assert false
      | (a::s2) -> 
        let op = EnvOp.find n envOp in 
          (op a)::s2 
    ) 
    | Branch c1 c2 -> (
      match s with 
        [] -> assert false
      | [a::s2] -> 
        if a
          then  eval_com c1 s
          else eval_com c2 s
    )
    | Cur c -> (
      match s with 
        [] -> assert false
      | [a::s2] -> (c,a)::s2 
    )
    | App -> (
      match s with 
        [] -> assert false
      | (((c, p), a)::s2) -> eval_com c ((p,a)::s2)
    )
    | Rplac -> (
      match s with  
        [] -> assert false
      | ((p,v)::p1::s) -> 
        if v === p1 
          then (p, p1)::s
          else assert false
    )