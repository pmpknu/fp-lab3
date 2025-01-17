open Interpolation.Utils

let string_of_points (points : point list) : string =
  let header = Printf.sprintf "%-10s | %-10s" "x" "y" in
  let separator = String.make (String.length header) '-' in
  let rows =
    points |> List.map (fun (x, y) -> Printf.sprintf "%-10.4f | %-10.4f" x y)
  in
  String.concat "\n" (header :: separator :: rows)

let%test "string_of_points" =
  let points = [ (1.57, 1.); (3.142, 0.) ] in
  string_of_points points
  = "x          | y         \n\
     -----------------------\n\
     1.5700     | 1.0000    \n\
     3.1420     | 0.0000    "

let string_of_algo (algo : algorithm) : string =
  match algo with
  | Lagrange -> "lagrange"
  | Linear -> "linear"
  | _ -> failwith "Invalid algorithm"
