open Core

(** Computes most profitable rods *)
let profit_rods prices len =
  let new_entry i j v = List.nth_exn prices (i - j) |> ( + ) v in
  let rec aux tbl =
    let i = List.length tbl in
    if i = len then tbl
    else
      let p = List.nth_exn prices i in
      let v' =
        tbl
        |> List.foldi ~init:Int.min_value ~f:(fun j acc v ->
               new_entry (i - 1) j v |> max acc)
        |> max p
      in
      tbl @ [ v' ] |> aux
  in
  let tbl = aux [] in
  tbl |> List.fold ~init:Int.min_value ~f:max
