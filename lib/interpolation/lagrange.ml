open Utils

let lagrange_basis i x xs =
  let n = List.length xs in
  List.fold_left
    (fun acc j ->
      if i = j then acc
      else acc *. (x -. List.nth xs j) /. (List.nth xs i -. List.nth xs j))
    1.0
    (List.init n (fun j -> j))

let lagrange_polynomial points x =
  let xs = List.map fst points in
  let ys = List.map snd points in
  let n = List.length points in
  List.fold_left
    (fun acc i -> acc +. (List.nth ys i *. lagrange_basis i x xs))
    0.0
    (List.init n (fun i -> i))

let lagrange_interpolation (step : float) (points : point list) (istart : point) =
  let interpolate = lagrange_polynomial points in
  let xs = generate_x_range step (istart) (List.hd (List.rev points)) in
  List.map (fun x -> (x, interpolate x)) xs

let%expect_test "lagrange_interpolation" =
  let points = [ (1.57, 1.); (3.142, 0.) ] in
  let interpolated = lagrange_interpolation 1. points  (1.57, 1.) in
  List.iter (fun (x, y) -> Printf.printf "%f %f\n" x y) interpolated;
  [%expect
    {|
    1.570000 1.000000
    2.570000 0.363868
    3.570000 -0.272265 |}]

let%expect_test "lagrange_interpolation" =
  let points = [ (0.0, 0.0); (1.571, 1.0); (3.142, 0.0); (4.712, -1.0) ] in
  let interpolated = lagrange_interpolation 1.0 points (0.0, 0.0) in
  List.iter (fun (x, y) -> Printf.printf "%f %f\n" x y) interpolated;
  [%expect
    {|
    0.000000 0.000000
    1.000000 0.973033
    2.000000 0.841202
    3.000000 0.120277
    4.000000 -0.673973
    5.000000 -1.025780 |}]
