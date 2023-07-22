open Lib
open Lib.Util

let solve () =
  let in_channel = open_in "data/67.txt" in
  let rec input_nums nums i =
    if i = 100
    then nums |> List.rev
    else (
      let ln =
        input_line in_channel |> String.split_on_char ' ' |> List.map int_of_string
      in
      input_nums (ln :: nums) (i + 1))
  in
  let nums = input_nums [] 0 in
  let height = List.length nums in
  let aux () =
    let rec aux' acc lvl =
      if lvl = height
      then acc
      else (
        let next =
          List.nth nums lvl
          |> List.mapi (fun i n ->
               let a = if i = 0 then 0 else List.nth acc (i - 1) in
               let b = if i = lvl then 0 else List.nth acc i in
               n + max a b)
        in
        aux' next (lvl + 1))
    in
    aux' (List.nth nums 0) 1 |> IntList.max
  in
  aux ()
;;
