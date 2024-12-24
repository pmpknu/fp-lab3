type t = {
  algorithms : Types.algorithm list;
  step : float;
}

let parse_algorithm = function
  | "linear" -> Types.Linear
  | "lagrange" -> Types.Lagrange
  | _ -> failwith "Unknown algorithm"

let parse () =
  let open Core in
  let linear = ref false in
  let lagrange = ref false in
  let step = ref 1.0 in

  let spec = Command.Spec.(
    empty
    +> flag "--linear" no_arg ~doc:"Use linear interpolation"
    +> flag "--lagrange" no_arg ~doc:"Use Lagrange interpolation"
    +> flag "--step" (optional_with_default 1.0 float) ~doc:"Sampling rate for the resulting data"
  ) in

  Command.Spec.parse spec (fun linear_flag lagrange_flag step_val ->
      linear := linear_flag;
      lagrange := lagrange_flag;
      step := step_val
    );

  let algorithms =
    let algs = [] in
    let algs = if !linear then Types.Linear :: algs else algs in
    let algs = if !lagrange then Types.Lagrange :: algs else algs in
    algs
  in

  if List.is_empty algorithms then
    failwith "No interpolation algorithm specified. Use --linear or --lagrange.";

  { algorithms; step = !step }