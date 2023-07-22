open Lib
open Lib.Util

let solve () =
  let rec aux res a b i =
    if b > 4000000
    then res
    else if i mod 3 = 1
    then aux (res + b) b (a + b) (i + 1)
    else aux res b (a + b) (i + 1)
  in
  aux 0 1 2 1
;;
