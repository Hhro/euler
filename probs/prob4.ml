open Lib
open Lib.Util

let solve () =
  let rec aux i j res =
    if i < 500
    then res
    else if j < 100
    then aux (i - 1) 999 res
    else (
      let prod = i * j in
      if prod > res && prod = (prod |> string_of_int |> String.rev |> int_of_string)
      then aux i (j - 1) prod
      else aux i (j - 1) res)
  in
  aux 999 999 0
;;
