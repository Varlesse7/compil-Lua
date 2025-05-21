use "types.ecl";;

let code = create<1024>();;

let load_code() =
  set(code, 0, Push);;
  set(code, 1, Quote(Int(42)));;
  set(code, 2, Cons);;
  set(code, 3, Cdr);;
