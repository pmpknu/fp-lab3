open Interpolation.Utils
open Cli.Loop

let step = ref 1.

let algo = ref Both

let set_step (value : float) =
  if value < 0. then raise (Arg.Bad "step must be positive")
  else step := value

let set_algo (value : string) =
  match value with
  | "lagrange" -> algo := Lagrange
  | "linear" -> algo := Linear
  | _ -> raise (Arg.Bad "invalid algorithm")

let speclist =
  [
    ("-step", Arg.Float set_step, "interpolation step");
    ("-algo", Arg.String set_algo, "interpolation algorithm. possible values: lagrange, linear");
  ]

let usage_msg = "interpolate -step <float step> [-algo <string algo>]"

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;

  print_endline "Initial points as <x>,<y>:";
  try
    loop !step !algo
  with
  | Failure msg ->
      Printf.printf "Error: %s\n" msg;
      exit 1
  | Sys.Break ->
      print_endline "Program stopped by a user";
      exit 0

