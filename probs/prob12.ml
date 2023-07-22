open Lib
open Lib.Util

let solve () =
  let rec aux nds n =
    let a = if n mod 2 = 0 then n / 2 else n in
    let b = if n mod 2 = 0 then n + 1 else (n + 1) / 2 in
    let nda = IntMap.find a nds in
    let ndb = if n mod 4 <> 1 then Math.num_of_divisors b else IntMap.find b nds in
    let nd = nda * ndb in
    if nd > 500 then a * b else aux (IntMap.add b ndb nds) (n + 1)
  in
  let nds = IntMap.empty |> IntMap.add 1 1 in
  aux nds 1
;;
