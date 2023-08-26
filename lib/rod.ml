open Core

(** Computes most profitable rods *)
let profit_rods prices len =
  let new_entry i j v =
    match List.nth prices (i - j) with None -> 0 | Some c -> c + v
  in
  let rec aux tbl =
    let i = List.length tbl in
    if i = len then tbl
    else
      let p =
        match List.nth prices i with
        | None -> raise (Invalid_argument "Unreachable")
        | Some c -> c
      in
      let v' =
        tbl
        |> List.foldi ~init:Int.min_value ~f:(fun j acc v ->
               new_entry (i - 1) j v |> max acc)
        |> max p
      in
      tbl @ [ v' ] |> aux
  in
  aux [] |> List.fold ~init:Int.min_value ~f:max
