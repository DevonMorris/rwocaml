open Core

(** Computes most profitable rods *)
let profit_rods prices len =
  let max_by_snd l =
    l
    |> List.max_elt ~compare:(fun x y -> compare_int (snd x) (snd y))
    |> Option.value_exn
  in
  let new_entry i j v = List.nth_exn prices (i - j) |> ( + ) v in
  (* tbl here is (len of last rod, total price) *)
  let rec aux tbl =
    let i = List.length tbl in
    if i = len then tbl
    else
      let p = List.nth_exn prices i in
      let tbl' =
        tbl |> List.mapi ~f:(fun j v -> (i - j, new_entry (i - 1) j (snd v)))
      in
      let tbl' = (i + 1, p) :: tbl' in
      let v' = tbl' |> max_by_snd in
      tbl @ [ v' ] |> aux
  in
  let tbl = aux [] in
  let max_v = tbl |> max_by_snd in
  (* compute the length of all rods *)
  let rec backtrack len_remain lens len_take =
    let len' = len_remain - len_take in
    if len' = 0 then len_take :: lens
    else backtrack len' (len_take :: lens) (fst (List.nth_exn tbl (len' - 1)))
  in
  (snd max_v, backtrack len [] (fst max_v))
