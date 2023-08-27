open Core

(** Computes the longest common subsequence *)
let lcs x y =
  let len_x = List.length x in
  let len_y = List.length y in
  let replace_last_row tbl row' =
    let n_rows = List.length tbl in
    let tbl' = List.slice tbl 0 (n_rows - 1) in
    tbl' @ [ row' ]
  in
  let rec build_table tbl i j =
    let x_i = List.nth_exn x (i - 1) in
    let y_j = List.nth_exn y (j - 1) in
    let row1 = List.nth_exn tbl (i - 1) in
    let row = List.nth_exn tbl i in
    let v' =
      let open Core.Poly in
      if x_i = y_j then
        let v = List.nth_exn row1 (j - 1) in
        v + 1
      else
        let v1 = List.nth_exn row1 j in
        let v2 = List.nth_exn row (j - 1) in
        max v1 v2
    in
    let row' = row @ [ v' ] in
    let tbl' = replace_last_row tbl row' in
    if i = len_x && j = len_y then tbl'
    else if j = len_y then
      let tbl' = tbl' @ [ [ 0 ] ] in
      build_table tbl' (i + 1) 1
    else build_table tbl' i (j + 1)
  in
  let tbl = [ List.init ~f:(const 0) (len_y + 1) ] @ [ [ 0 ] ] in
  let tbl = build_table tbl 1 1 in
  let rec backtrack seq tbl i j =
    if i = 0 || j = 0 then seq
    else
      let tbl_i_j = List.nth_exn (List.nth_exn tbl i) j in
      let tbl_i1_j = List.nth_exn (List.nth_exn tbl (i - 1)) j in
      let tbl_i_j1 = List.nth_exn (List.nth_exn tbl i) (j - 1) in
      let tbl_i1_j1 = List.nth_exn (List.nth_exn tbl (i - 1)) j in
      if tbl_i_j = tbl_i1_j then backtrack seq tbl (i - 1) j
      else if tbl_i_j = tbl_i_j1 then backtrack seq tbl i (j - 1)
      else if tbl_i_j = tbl_i1_j1 + 1 then
        let c = List.nth_exn x (i - 1) in
        let seq = c :: seq in
        backtrack seq tbl (i - 1) (j - 1)
      else raise (Failure "This should not occur")
  in
  let seq = backtrack [] tbl len_x len_y in
  (tbl |> List.last_exn |> List.last_exn, seq)
