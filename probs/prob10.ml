open Lib
open Lib.Util

let solve () =
  let rec aux n i =
    if i > 2000000
    then n
    else if Math.is_prime i
    then aux (n + i) (i + 2)
    else aux n (i + 2)
  in
  aux 2 3
;;
