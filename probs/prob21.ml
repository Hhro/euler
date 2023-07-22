open Lib
open Lib.Util

let solve () =
  let sum_of_d = Array.make 10001 1 in
  let rec aux i d =
    if d > 5000
    then ()
    else if i * d > 10000
    then aux 2 (d + 1)
    else (
      sum_of_d.(i * d) <- sum_of_d.(i * d) + d;
      aux (i + 1) d)
  in
  let rec sum_amicables acc i =
    if i = 10000
    then acc
    else (
      let sd = sum_of_d.(i) in
      if sd > 10000 || i <> sum_of_d.(sum_of_d.(i)) || i = sum_of_d.(i)
      then sum_amicables acc (i + 1)
      else (
        print_endline (string_of_int i);
        sum_amicables (acc + i) (i + 1)))
  in
  aux 2 2;
  sum_amicables 0 1
;;
