open Interpolation.Utils

let point_of_string (s : string) : point =
  match String.trim s |> String.split_on_char ',' with
  | x :: y :: _ -> (float_of_string x, float_of_string y)
  | _ -> failwith "Invalid point format"

let%test "point_of_string" = point_of_string "1.57, 1" = (1.57, 1.)

let read_point () : point =
  Sys.catch_break true;
  try read_line () |> point_of_string with
  | End_of_file -> failwith "End of file reached"
  | Sys.Break ->
      print_endline "Operation interrupted";
      exit 1
  | e -> raise e
