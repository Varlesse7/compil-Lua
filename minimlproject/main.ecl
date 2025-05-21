include "code.ecl";; (* Inclusion du fichier généré *)
use "types.ecl";;

let code = create<1024>();;
let stack = create<256>();;
let sp = ref 0;;
let env = create<256>();;
let ep = ref 0;;

let counter() =
  reg (fun c -> c + 1) init 0 ;;

let rec run_interp_at(ip : int) =
  let instr = get(code, ip) in
  match instr with

  | Quote(Int(n)) ->
      set(stack, !sp, Int(n));
      sp := !sp + 1;
      run_interp_at(ip + 1)

  | Quote(Bool(b)) ->
      set(stack, !sp, Bool(b));
      sp := !sp + 1;
      run_interp_at(ip + 1)

  | Push ->
      let top = get(stack, !sp - 1) in
      set(stack, !sp, top);
      sp := !sp + 1;
      run_interp_at(ip + 1)

  | Swap ->
      let a = get(stack, !sp - 1) in
      let b = get(stack, !sp - 2) in
      set(stack, !sp - 1, b);
      set(stack, !sp - 2, a);
      run_interp_at(ip + 1)

  | Car ->
      (match get(stack, !sp - 1) with
       | Pair(a, _) ->
           set(stack, !sp - 1, a);
           run_interp_at(ip + 1)
       | _ -> failwith "Expected pair for Car")

  | Cdr ->
      (match get(stack, !sp - 1) with
       | Pair(_, b) ->
           set(stack, !sp - 1, b);
           run_interp_at(ip + 1)
       | _ -> failwith "Expected pair for Cdr")

  | Cons ->
      let v1 = get(stack, !sp - 2) in
      let v2 = get(stack, !sp - 1) in
      set(stack, !sp - 2, Pair(v1, v2));
      sp := !sp - 1;
      run_interp_at(ip + 1)

  | Rplac ->
      let env_val = get(stack, !sp - 2) in
      let replacement = get(stack, !sp - 1) in
      let replaced = rplac_subst NullValue replacement env_val in
      set(stack, !sp - 2, Pair(replaced, replacement));
      sp := !sp - 1;
      run_interp_at(ip + 1)

  | Op(Add) ->
      let Int(b) = get(stack, !sp - 1) in
      let Int(a) = get(stack, !sp - 2) in
      set(stack, !sp - 2, Int(a + b));
      sp := !sp - 1;
      run_interp_at(ip + 1)

  | Op(Sub) ->
      let Int(b) = get(stack, !sp - 1) in
      let Int(a) = get(stack, !sp - 2) in
      set(stack, !sp - 2, Int(a - b));
      sp := !sp - 1;
      run_interp_at(ip + 1)

  | Op(Mult) ->
      let Int(b) = get(stack, !sp - 1) in
      let Int(a) = get(stack, !sp - 2) in
      set(stack, !sp - 2, Int(a * b));
      sp := !sp - 1;
      run_interp_at(ip + 1)

  | Branch(i1, i2) ->
      let Bool(b) = get(stack, !sp - 1) in
      sp := !sp - 1;
      if b then run_interp_at(i1) else run_interp_at(i2)

  | Cur i ->
      set(stack, !sp, Closure(i, !ep));
      sp := !sp + 1;
      run_interp_at(ip + 1)

  | App ->
      let Closure(ip', env') = get(stack, !sp - 2) in
      let arg = get(stack, !sp - 1) in
      set(env, env', arg);
      sp := !sp - 2;
      run_interp_at(ip')

  | _ -> ()

let run_interp() = run_interp_at(0);;

let main (bouton : bool) =
  let cy = counter() in
  let (v, rdy) = exec
    load_code();
    print_string "start execution at ";
    print_int cy;
    print_string " !";
    print_newline();
    run_interp();
    print_string "execution is finished at ";
    print_int cy;
    print_string " !";
    print_newline();
    42
  in
  let green_led = not(rdy) in
  green_led;;
