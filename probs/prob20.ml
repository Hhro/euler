open Lib
open Lib.Util

let solves () =
  let aux () =
    let divisor = Big_int_Z.big_int_of_int 10 in
    let zero = Big_int_Z.zero_big_int in
    let rec fact n =
      if n = 0
      then Big_int_Z.big_int_of_int 1
      else Big_int_Z.mult_big_int (Big_int_Z.big_int_of_int n) (fact (n - 1))
    in
    let rec sum acc n =
      if Big_int_Z.eq_big_int n zero
      then acc
      else
        sum
          (Big_int_Z.add_big_int acc (Big_int_Z.mod_big_int n divisor))
          (Big_int_Z.div_big_int n divisor)
    in
    sum zero (fact 100) |> Big_int_Z.int_of_big_int
  in
  aux ()
;;
