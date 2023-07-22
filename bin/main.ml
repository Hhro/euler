open Prob

let () =
  let t = Sys.time () in
  let answer = Prob21.solve () in
  Format.printf "elapsed: %f" (Sys.time () -. t);
  answer |> string_of_int |> print_endline
;;
