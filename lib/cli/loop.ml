open Interpolation.Utils
open Interpolation.Lagrange
open Interpolation.Linear
open Input
open Output

let execute_interpolate algo interpolation step points start =
  print_endline (string_of_algo algo);
  let interpolated_points = interpolation step points start in
  print_endline (string_of_points interpolated_points);
  get_last interpolated_points

let loop (step : float) (algo : algorithm) =
  let point1 = read_point () in
  let point2 = read_point () in
  let points = [ point1; point2 ] in

  let rec looprec (points : point list) (start_linear : point) (start_lagrange : point) =
    let new_start_linear, new_start_lagrange =
      match algo with
      | Both ->
          let linear_result =
            execute_interpolate Linear linear_interpolation step points start_linear
          in
          let lagrange_result =
            if List.length points > 3 then
              execute_interpolate Lagrange lagrange_interpolation step points start_lagrange
            else
              start_lagrange
          in
          (linear_result, lagrange_result)
      | Lagrange ->
          if List.length points > 3 then
            let lagrange_result =
              execute_interpolate algo lagrange_interpolation step points start_lagrange
            in
            (start_linear, lagrange_result)
          else
            (start_linear, start_lagrange)
      | Linear ->
          let linear_result =
            execute_interpolate algo linear_interpolation step points start_linear
          in
          (linear_result, start_lagrange)
    in
    let new_point = read_point () in
    looprec
      (if List.length points > 3 then
        List.tl points @ [ new_point ]
      else
        points @ [ new_point ])
      new_start_linear
      new_start_lagrange
  in
  looprec points (List.hd points) (List.hd points)
