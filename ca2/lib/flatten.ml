open Mltocam

(* Type plat *)
type flat_com =
  | FQuote of value
  | FOp of operator
  | FCar
  | FCdr
  | FCons
  | FPush
  | FSwap
  | FApp
  | FRplac
  | FBranch of int * int
  | FCur of int

let flatten (code : com list) : flat_com array =
  let instructions = ref [] in
  let table = Hashtbl.create 64 in

  let rec aux code =
    if Hashtbl.mem table code then Hashtbl.find table code
    else
      let pos = List.length !instructions in
      Hashtbl.add table code pos;

      let rec flatten_code (cmds : coms) =
        match cmds with
        | [] -> []
        | Quote v :: rest -> FQuote v :: flatten_code rest
        | Op op :: rest -> FOp op :: flatten_code rest
        | Car :: rest -> FCar :: flatten_code rest
        | Cdr :: rest -> FCdr :: flatten_code rest
        | Cons :: rest -> FCons :: flatten_code rest
        | Push :: rest -> FPush :: flatten_code rest
        | Swap :: rest -> FSwap :: flatten_code rest
        | App :: rest -> FApp :: flatten_code rest
        | Rplac :: rest -> FRplac :: flatten_code rest
        | Cur c :: rest ->
            let i = aux c in
            FCur i :: flatten_code rest
        | Branch(c1, c2) :: rest ->
            let i1 = aux c1 in
            let i2 = aux c2 in
            FBranch (i1, i2) :: flatten_code rest
      in

      let flat = flatten_code code in
      instructions := !instructions @ flat;
      pos
  in

  ignore (aux code);
  Array.of_list !instructions


let string_of_flat = function
  | FQuote (Int n) -> Printf.sprintf "Quote(Int(%d))" n
  | FQuote (Bool b) -> Printf.sprintf "Quote(Bool(%b))" b
  | FQuote _ -> "Quote(...)"
  | FOp Add -> "Op(Add)"
  | FOp Sub -> "Op(Sub)"
  | FOp Mult -> "Op(Mult)"
  | FCar -> "Car"
  | FCdr -> "Cdr"
  | FCons -> "Cons"
  | FPush -> "Push"
  | FSwap -> "Swap"
  | FApp -> "App"
  | FRplac -> "Rplac"
  | FBranch(i1, i2) -> Printf.sprintf "Branch(%d, %d)" i1 i2
  | FCur i -> Printf.sprintf "Cur(%d)" i


let generate_eclat code =
  Printf.printf "let code = create<1024>();;\n\n";
  Printf.printf "let load_code() =\n";
  Array.iteri (fun i instr ->
    Printf.printf "  set(code, %d, %s);;\n" i (string_of_flat instr)
  ) code