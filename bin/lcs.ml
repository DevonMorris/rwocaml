open Rwo.Lcs

let test_l1 = [ "B"; "C"; "D"; "B"; "C"; "D"; "A" ]
let test_l2 = [ "A"; "B"; "E"; "C"; "B"; "A" ]
let v, seq = lcs test_l1 test_l2
let () = Printf.printf "Len lcs: %d\n" v
let () = Printf.printf "Seq: "
let () = List.iter (fun s -> Printf.printf "%s, " s) seq
let () = Printf.printf "\n"
