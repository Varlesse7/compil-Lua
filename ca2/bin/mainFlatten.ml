open Mltocam  (* ou ton module CAM *)
open Flatten

module StringMap = Map.Make(String)

(* Exemple concret *)

let () =
  let expr = If (True, Number 1, Number 2) in
  let cam_code = compile StringMap.empty expr in
  let flat = flatten cam_code in
  Printf.printf "Code CAM plat :\n";
  Array.iteri (fun i instr ->
    Printf.printf "%2d: %s\n" i (string_of_flat instr)
  ) flat;
  Printf.printf "\nCode Éclat généré :\n";
  generate_eclat flat
