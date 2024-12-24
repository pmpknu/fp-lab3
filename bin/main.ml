open Core
open Io
open Interpolation

let generate_points ~start ~stop ~step =
  List.init (Float.to_int ((stop -. start) /. step) + 1) ~f:(fun i -> start +. step *. Float.of_int i)

let interpolate_points config points =
  let start = fst (List.hd_exn points) in
  let stop = fst (List.last_exn points) in
  let x_new = generate_points ~start ~stop ~step:config.Config.step in
  List.concat_map config.Config.algorithms ~f:(fun algorithm ->
    List.map x_new ~f:(fun x -> (x, interpolate algorithm points x)))

(** Main loop for processing input/output *)
let rec main_loop config points =
  match In_channel.input_line In_channel.stdin with
  | Some line ->
      let new_point = parse_line line in
      let new_points = points @ [new_point] in
      let interpolated_points = interpolate_points config new_points in
      write_output interpolated_points;
      main_loop config new_points
  | None -> () (* End of input *)

let () =
  let config = Config.parse () in
  main_loop config []