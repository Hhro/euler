let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let lcm ns = ns |> List.fold_left (fun res n -> res * n / gcd res n) 1
