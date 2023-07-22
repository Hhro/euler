open Lib
open Lib.Util

let solve () =
  let aux () =
    let base = Big_int_Z.big_int_of_int 2 in
    let divisor = Big_int_Z.big_int_of_int 10 in
    let zero = Big_int_Z.zero_big_int in
    let rec mul acc i =
      if i = 1000 then acc else mul (Big_int_Z.mult_big_int base acc) (i + 1)
    in
    let rec sum acc n =
      if Big_int_Z.eq_big_int n zero
      then acc
      else
        sum
          (Big_int_Z.add_big_int acc (Big_int_Z.mod_big_int n divisor))
          (Big_int_Z.div_big_int n divisor)
    in
    sum zero (mul base 1) |> Big_int_Z.int_of_big_int
  in
  aux ()
;;
