open Core

let lis l =
  let max_by_snd (v, c) (v', c') = if c' > c then (v', c') else (v, c) in
  let gt_filt hd tbl = List.filter ~f:(fun (v, _) -> hd > v) tbl in
  let best_elem hd tbl =
    match gt_filt hd tbl with
    | [] -> (hd, 0)
    | l -> List.fold_left ~init:(0, 0) ~f:max_by_snd l
  in
  let rec aux l tbl =
    match l with
    | [] -> tbl
    | hd :: tl ->
        let _, c = best_elem hd tbl in
        let tbl' = (hd, c + 1) :: tbl in
        aux tl tbl'
  in
  List.fold_left ~init:(0, 0) ~f:max_by_snd (aux l []) |> snd
