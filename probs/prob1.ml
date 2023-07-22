open Lib
open Lib.Util

let solve () =
  let rec aux i =
    if i >= 1000
    then 0
    else if i mod 3 = 0 || i mod 5 = 0
    then i + aux (i + 1)
    else aux (i + 1)
  in
  aux 3
;;
