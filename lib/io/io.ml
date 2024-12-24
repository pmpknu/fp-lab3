let read_line () =
  try Some (input_line stdin)
  with End_of_file -> None

let read_all () =
  let rec aux acc =
    match read_line () with
    | Some line -> aux (line :: acc)
    | None -> List.rev acc
  in
  aux []

let write_line line =
  output_string stdout (line ^ "\n");
  flush stdout

let write_all lines =
  List.iter write_line lines

let parse_line line =
  match String.split_on_char ',' line with
  | [x; y] -> (float_of_string x, float_of_string y)
  | _ -> failwith "Invalid input format"

let read_input () =
  read_all () |> List.map parse_line

let write_output points =
  points
  |> List.map (fun (x, y) -> Printf.sprintf "%f,%f" x y)
  |> write_all

let%test "parse_line" =
  parse_line "1.0,2.0" = (1.0, 2.0)

let%test "parse_line_fail" =
  try
    let _ = parse_line "1.0" in false
  with
  | Failure _ -> true
  | _ -> false