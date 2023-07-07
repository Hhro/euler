open Euler
open Euler.Util

let chall_1 () =
  let rec solve i =
    if i >= 1000
    then 0
    else if i mod 3 = 0 || i mod 5 = 0
    then i + solve (i + 1)
    else solve (i + 1)
  in
  solve 3
;;

let chall_2 () =
  let rec solve res a b i =
    if b > 4000000
    then res
    else if i mod 3 = 1
    then solve (res + b) b (a + b) (i + 1)
    else solve res b (a + b) (i + 1)
  in
  solve 0 1 2 1
;;

let chall_3 () =
  let rec solve n i =
    if i * i > n then n else if n mod i = 0 then solve (n / i) i else solve n (i + 1)
  in
  solve 600851475143 2
;;

let chall_4 () =
  let rec solve i j res =
    if i < 500
    then res
    else if j < 100
    then solve (i - 1) 999 res
    else (
      let prod = i * j in
      if prod > res && prod = (prod |> string_of_int |> String.rev |> int_of_string)
      then solve i (j - 1) prod
      else solve i (j - 1) res)
  in
  solve 999 999 0
;;

let chall_5 () = Math.lcm (List.range 1 21)
let chall_6 () = 100 * 101 * 302 * 99 / 12

let chall_7 () =
  let rec solve cnt n =
    if cnt = 200000
    then n - 2
    else n + 2 |> if Math.is_prime n then solve (cnt + 1) else solve cnt
  in
  solve 1 3
;;

let chall_8 () =
  let series =
    "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
  in
  let rec solve res i =
    if i > 1000 - 13
    then res
    else (
      let n =
        String.sub series i 13
        |> String.fold_left (fun acc c -> (Char.code c - 0x30) * acc) 1
      in
      solve (max res n) (i + 1))
  in
  solve 0 0
;;

let chall_9 () =
  let rec solve a b c =
    if a > 1000
    then 0
    else if b > 1000
    then solve (a + 1) (a + 2) (a + 3)
    else if c > 1000
    then solve a (b + 1) (b + 2)
    else (
      let sum = a + b + c in
      if sum = 1000 && (a * a) + (b * b) = c * c then a * b * c else solve a b (c + 1))
  in
  solve 1 2 3
;;

let chall_10 () =
  let rec solve n i =
    if i > 2000000
    then n
    else if Math.is_prime i
    then solve (n + i) (i + 2)
    else solve n (i + 2)
  in
  solve 2 3
;;

let chall_11 () =
  let grid =
    [| [| 8; 2; 22; 97; 38; 15; 0; 40; 0; 75; 4; 5; 7; 78; 52; 12; 50; 77; 91; 8 |]
     ; [| 49; 49; 99; 40; 17; 81; 18; 57; 60; 87; 17; 40; 98; 43; 69; 48; 4; 56; 62; 0 |]
     ; [| 81; 49; 31; 73; 55; 79; 14; 29; 93; 71; 40; 67; 53; 88; 30; 3; 49; 13; 36; 65 |]
     ; [| 52; 70; 95; 23; 4; 60; 11; 42; 69; 24; 68; 56; 1; 32; 56; 71; 37; 2; 36; 91 |]
     ; [| 22
        ; 31
        ; 16
        ; 71
        ; 51
        ; 67
        ; 63
        ; 89
        ; 41
        ; 92
        ; 36
        ; 54
        ; 22
        ; 40
        ; 40
        ; 28
        ; 66
        ; 33
        ; 13
        ; 80
       |]
     ; [| 24; 47; 32; 60; 99; 3; 45; 2; 44; 75; 33; 53; 78; 36; 84; 20; 35; 17; 12; 50 |]
     ; [| 32
        ; 98
        ; 81
        ; 28
        ; 64
        ; 23
        ; 67
        ; 10
        ; 26
        ; 38
        ; 40
        ; 67
        ; 59
        ; 54
        ; 70
        ; 66
        ; 18
        ; 38
        ; 64
        ; 70
       |]
     ; [| 67; 26; 20; 68; 2; 62; 12; 20; 95; 63; 94; 39; 63; 8; 40; 91; 66; 49; 94; 21 |]
     ; [| 24; 55; 58; 5; 66; 73; 99; 26; 97; 17; 78; 78; 96; 83; 14; 88; 34; 89; 63; 72 |]
     ; [| 21; 36; 23; 9; 75; 0; 76; 44; 20; 45; 35; 14; 0; 61; 33; 97; 34; 31; 33; 95 |]
     ; [| 78; 17; 53; 28; 22; 75; 31; 67; 15; 94; 3; 80; 4; 62; 16; 14; 9; 53; 56; 92 |]
     ; [| 16; 39; 5; 42; 96; 35; 31; 47; 55; 58; 88; 24; 0; 17; 54; 24; 36; 29; 85; 57 |]
     ; [| 86; 56; 0; 48; 35; 71; 89; 7; 5; 44; 44; 37; 44; 60; 21; 58; 51; 54; 17; 58 |]
     ; [| 19; 80; 81; 68; 5; 94; 47; 69; 28; 73; 92; 13; 86; 52; 17; 77; 4; 89; 55; 40 |]
     ; [| 4; 52; 8; 83; 97; 35; 99; 16; 7; 97; 57; 32; 16; 26; 26; 79; 33; 27; 98; 66 |]
     ; [| 88; 36; 68; 87; 57; 62; 20; 72; 3; 46; 33; 67; 46; 55; 12; 32; 63; 93; 53; 69 |]
     ; [| 4; 42; 16; 73; 38; 25; 39; 11; 24; 94; 72; 18; 8; 46; 29; 32; 40; 62; 76; 36 |]
     ; [| 20; 69; 36; 41; 72; 30; 23; 88; 34; 62; 99; 69; 82; 67; 59; 85; 74; 4; 36; 16 |]
     ; [| 20; 73; 35; 29; 78; 31; 90; 1; 74; 31; 49; 71; 48; 86; 81; 16; 23; 57; 5; 54 |]
     ; [| 1; 70; 54; 71; 83; 51; 54; 69; 16; 92; 33; 48; 61; 43; 52; 1; 89; 19; 67; 48 |]
    |]
  in
  let get x y = if x < 0 || x >= 20 || y < 0 || y >= 20 then 0 else grid.(y).(x) in
  let rec solve acc x y =
    let rec calc_muls muls x y n =
      if n = 4
      then muls
      else
        calc_muls
          [ List.nth muls 0 * get (x + n) y
          ; List.nth muls 1 * get x (y + n)
          ; List.nth muls 2 * get (x + n) (y + n)
          ; List.nth muls 3 * get (x + n) (y - n)
          ]
          x
          y
          (n + 1)
    in
    if x = 0 && y = 20
    then acc
    else (
      let muls = calc_muls [ 1; 1; 1; 1 ] x y 0 in
      let acc = max acc (List.fold_left max min_int muls) in
      if x = 19 then solve acc 0 (y + 1) else solve acc (x + 1) y)
  in
  solve 0 0 0
;;

let chall_12 () =
  let rec solve nds n =
    let a = if n mod 2 = 0 then n / 2 else n in
    let b = if n mod 2 = 0 then n + 1 else (n + 1) / 2 in
    let nda = IntMap.find a nds in
    let ndb = if n mod 4 <> 1 then Math.num_of_divisors b else IntMap.find b nds in
    let nd = nda * ndb in
    if nd > 500 then a * b else solve (IntMap.add b ndb nds) (n + 1)
  in
  let nds = IntMap.empty |> IntMap.add 1 1 in
  solve nds 1
;;

let () =
  let t = Sys.time () in
  let answer = chall_12 () in
  Format.printf "elapsed: %f" (Sys.time () -. t);
  answer |> string_of_int |> print_endline
;;
