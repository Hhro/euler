open Lib
open Lib.Util

let solve () =
  let rec aux a b c =
    if a > 1000
    then 0
    else if b > 1000
    then aux (a + 1) (a + 2) (a + 3)
    else if c > 1000
    then aux a (b + 1) (b + 2)
    else (
      let sum = a + b + c in
      if sum = 1000 && (a * a) + (b * b) = c * c then a * b * c else aux a b (c + 1))
  in
  aux 1 2 3
;;
