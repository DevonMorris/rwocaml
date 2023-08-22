open List

let lis l =
  let max_by_snd = fun (v, c) (v', c') -> if c' > c then (v', c') else (v, c) in
  let gt_filt hd tbl= List.filter (fun (v, _) -> hd > v) tbl in
  let best_elem hd tbl = match gt_filt hd tbl with
    | [] -> (hd, 0)
    | l -> List.fold_left max_by_snd (0, 0) l
  in
  let rec aux l tbl =
    match l with
    | [] -> tbl
    | hd :: tl -> let (_, c) = best_elem hd tbl in
                  let tbl' = (hd, c + 1) :: tbl in
                  aux tl tbl'
  in
  snd (List.fold_left max_by_snd (0, 0) (aux l []))

let test = [5; 7; 4; -3; 9; 1; 10; 4; 5; 8; 9; 3]

let () =
  Printf.printf "%i\n" (lis test)
