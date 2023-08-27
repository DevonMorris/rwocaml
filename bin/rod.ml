open Core
open Rwo.Rod

let prices = [ 1; 5; 8; 9; 10; 17; 17; 20; 24; 30 ]
let len = 8
let profit, lens = profit_rods prices len
let () = Printf.printf "Profit: %i\n" profit
let () = Printf.printf "Rod Lengths:\n"
let () = List.iter ~f:(fun x -> Printf.printf "%i\n" x) lens
