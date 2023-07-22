open Lib
open Lib.Util

let solve () =
  let rec aux cnt n =
    if cnt = 200000
    then n - 2
    else n + 2 |> if Math.is_prime n then aux (cnt + 1) else aux cnt
  in
  aux 1 3
;;
