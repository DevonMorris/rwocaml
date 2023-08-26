open Core
open Stdio

let rec read_and_accumulate_sum accum =
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> accum
  | Some x -> read_and_accumulate_sum (accum +. Float.of_string x)

let () = printf "Total: %F\n" (read_and_accumulate_sum 0.)
