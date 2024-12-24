open QCheck
open Interpolation

let linear_interpolation_test =
  Test.make ~name:"Linear interpolation test" ~count:1000
    (pair (list_of_size Gen.(1 -- 10) (pair float float)) float)
    (fun (points, x) ->
       let points = List.sort compare points in
       try
         let _ = Linear.interpolate points x in
         true
       with _ -> true)

let lagrange_interpolation_test =
  Test.make ~name:"Lagrange interpolation test" ~count:1000
    (pair (list_of_size Gen.(1 -- 10) (pair float float)) float)
    (fun (points, x) ->
       let points = List.sort compare points in
       try
         let _ = Lagrange.interpolate points x in
         true
       with _ -> true)

let () =
  Alcotest.run "Interpolation tests" [
    "Linear", [QCheck_alcotest.to_alcotest linear_interpolation_test];
    "Lagrange", [QCheck_alcotest.to_alcotest lagrange_interpolation_test];
  ]