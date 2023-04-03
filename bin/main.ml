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

let () = chall_2 |> string_of_int |> print_endline
