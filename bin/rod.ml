open Rwo.Rod

let prices = [ 1; 5; 8; 9; 10; 17; 17; 20; 24; 30 ]
let len = 10
let () = Printf.printf "%i\n" (profit_rods prices len)
