let rec gcd a b = if b = 0 then a else gcd b (a mod b)
let lcm ns = ns |> List.fold_left (fun res n -> res * n / gcd res n) 1
let sum ns = ns |> List.fold_left (fun res n -> res + n) 0

let pow b e =
  let rec aux res i = if i = e then res else aux (res * b) (i + 1) in
  aux 1 0
;;

let powm b e m =
  let rec aux r x y =
    if y = 0
    then r
    else (
      let r = if y mod 2 = 1 then r * x mod m else r in
      let x = x * x mod m in
      let y = y / 2 in
      aux r x y)
  in
  aux 1 (b mod m) e
;;

let is_prime n =
  let rec brute d n =
    if d * d > n then true else if n mod d = 0 then false else brute (d + 1) n
  in
  let rec miller d n a =
    if d mod 2 <> 0
    then (
      let crit = powm a d n in
      crit = n - 1 || crit = 1)
    else if powm a d n = n - 1
    then true
    else miller (d / 2) n a
  in
  if n = 1
  then false
  else if n = 2
  then true
  else if n <= 10000
  then brute 2 n
  else if n < pow 2 31
  then [ 2; 7; 61 ] |> List.fold_left (fun res a -> res && miller (n - 1) n a) true
  else failwith "miller: not support number not in 32-bit int range"
;;

let num_of_divisors n =
  let cbrt = Float.cbrt (n |> float_of_int) |> int_of_float in
  let rec cnt acc div d =
    if div mod d = 0 && div <> 0 then cnt (acc + 1) (div / d) d else div, acc
  in
  let rec nd_under_cbrt acc n i =
    if n = 1
    then acc, 1
    else if n mod i <> 0 || not (is_prime i)
    then nd_under_cbrt acc n (i + 1)
    else (
      let div, e = cnt 0 n i in
      if i <= cbrt then nd_under_cbrt (acc * (e + 1)) div (i + 1) else acc, n)
  in
  let nd, y = nd_under_cbrt 1 n 1 in
  if y = 1
  then nd
  else if is_prime y
  then nd * 2
  else (
    let sqrt = Float.sqrt (y |> float_of_int) |> int_of_float in
    if y = sqrt * sqrt && is_prime sqrt then nd * 3 else nd * 4)
;;
