(** Выполняет интерполяцию Лагранжа для списка точек. *)
let interpolate points x =
  let basis j =
    List.fold_left (fun acc (xk, _) ->
        if xk = fst (List.nth points j) then acc
        else acc *. (x -. xk) /. (fst (List.nth points j) -. xk))
      1.0 points
  in
  let rec index_of elem lst idx =
    match lst with
    | [] -> raise Not_found
    | hd :: tl -> if hd = elem then idx else index_of elem tl (idx + 1)
  in
  List.fold_left (fun acc (xj, yj) -> acc +. yj *. basis (index_of (xj, yj) points 0)) 0.0 points

(* Inline tests *)
let%test "interpolate_lagrange" =
  let points = [(0.0, 0.0); (1.0, 1.0); (2.0, 0.0)] in
  Float.equal (interpolate points 0.5) 0.75

let%test "interpolate_lagrange_single_point" =
  let points = [(1.0, 2.0)] in
  Float.equal (interpolate points 1.0) 2.0
