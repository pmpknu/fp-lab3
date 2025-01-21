open Interpolation.Utils
open Interpolation.Lagrange
open Interpolation.Linear
open Input
open Output

let execute_interpolate algo intepolation step points =
  print_endline (string_of_algo algo);
  intepolation step points |> string_of_points |> print_endline

let loop (step : float) (algo : algorithm) =
  let point1 = read_point () in
  let point2 = read_point () in
  let points = [ point1; point2 ] in

  let rec looprec (points : point list) =
    (match algo with
    | Both ->
        execute_interpolate Linear linear_interpolation step points;
        if List.length points > 3 then
          execute_interpolate Lagrange lagrange_interpolation step points
    | Lagrange ->
        if List.length points > 3 then
          execute_interpolate algo lagrange_interpolation step points
    | Linear -> execute_interpolate algo linear_interpolation step points);

    let new_point = read_point () in
    looprec (
      if List.length points > 3 then
        List.tl points @ [ new_point ]
      else 
        points @ [ new_point ]
      )
  in
  looprec points
