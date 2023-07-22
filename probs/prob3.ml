open Lib
open Lib.Util

let solve () =
  let rec aux n i =
    if i * i > n then n else if n mod i = 0 then aux (n / i) i else aux n (i + 1)
  in
  aux 600851475143 2
;;
