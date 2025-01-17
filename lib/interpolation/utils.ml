type point = float * float
type algorithm = Lagrange | Linear | Both

let generate_x_range (step : float) (start : point) (target : point) :
    float list =
  let start_x, _ = start in
  let target_x, _ = target in
  let rec aux x acc =
    if x > target_x +. step then acc else aux (x +. step) (x :: acc)
  in
  List.rev (aux start_x [])

let%expect_test "generate_x_range" =
  let start = (1.57, 1.) in
  let target = (3.142, 0.) in
  let x_range = generate_x_range 1. start target in
  List.iter (fun x -> Printf.printf "%f\n" x) x_range;
  [%expect {|
    1.570000
    2.570000
    3.570000 |}]

let get_last_two lst =
  match List.rev lst with
  | x1 :: x2 :: _ -> (x2, x1)
  | _ -> failwith "there should be at least two elements in the list"