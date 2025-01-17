open Interpolation.Utils

let string_of_points (points : point list) : string =
  let xs = points |> List.map (fun (x, _) -> string_of_float x) in
  let ys = points |> List.map (fun (_, y) -> string_of_float y) in
  let xs_str = String.concat " " xs in
  let ys_str = String.concat " " ys in
  xs_str ^ "\n" ^ ys_str

let%test "string_of_points" =
  let points = [ (1.57, 1.); (3.142, 0.) ] in
  string_of_points points = "1.57 3.142\n1. 0."

let string_of_algo (algo : algorithm) : string =
  match algo with
  | Lagrange -> "lagrange"
  | Linear -> "linear"
  | _ -> failwith "Invalid algorithm"
