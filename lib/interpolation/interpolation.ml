module type S = sig
  val interpolate : (float * float) list -> float -> float
end

module Linear : S = struct
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
    end

module Lagrange : S = struct
    let interpolate points x =
      let rec find_segment = function
        | (x1, y1) :: (x2, y2) :: _ when x1 <= x && x <= x2 ->
            let t = (x -. x1) /. (x2 -. x1) in
            y1 +. t *. (y2 -. y1)
        | _ :: rest -> find_segment rest
        | _ -> failwith "x is out of the interpolation range"
      in
      find_segment points
    end

let%test "interpolate_linear" =
  let points = [(0.0, 0.0); (1.0, 1.0); (2.0, 0.0)] in
  Linear.interpolate points 0.5 = 0.5

let%test "interpolate_linear_out_of_range" =
  let points = [(0.0, 0.0); (1.0, 1.0); (2.0, 0.0)] in
  try
    let _ = Linear.interpolate points 3.0 in false
  with
  | Failure _ -> true
  | _ -> false


let%test "interpolate_lagrange" =
  let points = [(0.0, 0.0); (1.0, 1.0); (2.0, 0.0)] in
  Float.equal (Lagrange.interpolate points 0.5) 0.75

let%test "interpolate_lagrange_single_point" =
  let points = [(1.0, 2.0)] in
  Float.equal (Lagrange.interpolate points 1.0) 2.0
