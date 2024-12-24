let interpolate points x =
  let rec find_segment = function
    | (x1, y1) :: (x2, y2) :: _ when x1 <= x && x <= x2 ->
        let t = (x -. x1) /. (x2 -. x1) in
        y1 +. t *. (y2 -. y1)
    | _ :: rest -> find_segment rest
    | _ -> failwith "x is out of the interpolation range"
  in
  find_segment points

(* Inline tests *)
let%test "interpolate_linear" =
  let points = [(0.0, 0.0); (1.0, 1.0); (2.0, 0.0)] in
  interpolate points 0.5 = 0.5

let%test "interpolate_linear_out_of_range" =
  let points = [(0.0, 0.0); (1.0, 1.0); (2.0, 0.0)] in
  try
    let _ = interpolate points 3.0 in false
  with
  | Failure _ -> true
  | _ -> false