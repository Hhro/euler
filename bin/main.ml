open Euler
open Euler.Util

let chall_1 =
  let rec solve i =
    if i >= 1000 then 0
    else if i mod 3 = 0 || i mod 5 = 0 then i + solve (i + 1)
    else solve (i + 1)
  in
  solve 3

let chall_2 =
  let rec solve res a b i =
    if b > 4000000 then res
    else if i mod 3 = 1 then solve (res + b) b (a + b) (i + 1)
    else solve res b (a + b) (i + 1)
  in
  solve 0 1 2 1

let chall_3 =
  let rec solve n i =
    if i * i > n then n
    else if n mod i = 0 then solve (n / i) i
    else solve n (i + 1)
  in
  solve 600851475143 2

let chall_4 =
  let rec solve i j res =
    if i < 500 then res
    else if j < 100 then solve (i - 1) 999 res
    else
      let prod = i * j in
      if
        prod > res
        && prod = (prod |> string_of_int |> String.rev |> int_of_string)
      then solve i (j - 1) prod
      else solve i (j - 1) res
  in
  solve 999 999 0

let chall_5 = Math.lcm (List.range 1 21)

let () = chall_5 |> string_of_int |> print_endline
