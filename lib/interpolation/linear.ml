open Utils

let linear_interpolation (step : float) (points : point list) (istart : point) :
    point list =
  let start = istart in
  let target = get_last points in
  let x1, y1 = start in
  let x2, y2 = target in
  let slope = (y2 -. y1) /. (x2 -. x1) in
  let rec loop x acc =
    if x < x2 +. step then
      loop (x +. step) ((x, y1 +. (slope *. (x -. x1))) :: acc)
    else acc
  in
  List.rev (loop x1 [])

let%expect_test "linear interpolation" =
  let points = [ (1.57, 1.); (3.142, 0.) ] in
  let interpolated = linear_interpolation 1. points (1.57, 1.) in
  List.iter (fun (x, y) -> Printf.printf "(%f, %f)\n" x y) interpolated;
  [%expect
    {|
    (1.570000, 1.000000)
    (2.570000, 0.363868)
    (3.570000, -0.272265) |}]

let%expect_test "linear interpolation" =
  let points = [ (4.712, -1.); (12.568, 0.) ] in
  let interpolated = linear_interpolation 1. points (4.712, -1.) in
  List.iter (fun (x, y) -> Printf.printf "(%f, %f)\n" x y) interpolated;
  [%expect
    {|
    (4.712000, -1.000000)
    (5.712000, -0.872709)
    (6.712000, -0.745418)
    (7.712000, -0.618126)
    (8.712000, -0.490835)
    (9.712000, -0.363544)
    (10.712000, -0.236253)
    (11.712000, -0.108961)
    (12.712000, 0.018330) |}]
