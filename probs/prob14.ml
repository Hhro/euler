open Lib
open Lib.Util

let solve () =
  let rec collatz tbl seq n =
    match Hashtbl.find_opt tbl n with
    | Some l -> Iter.of_list seq |> Iter.iteri (fun i x -> Hashtbl.add tbl x (l + i + 1))
    | None ->
      if n mod 2 = 0
      then collatz tbl (n :: seq) (n / 2)
      else collatz tbl (n :: seq) ((3 * n) + 1)
  in
  let rec aux (sn, ml) tbl n =
    if n = 1000000
    then sn
    else (
      collatz tbl [] n;
      let l = Hashtbl.find tbl n in
      aux (if l > ml then n, l else sn, ml) tbl (n + 1))
  in
  let collatz_tbl = Hashtbl.create 3000000 in
  Hashtbl.add collatz_tbl 1 1;
  aux (0, 0) collatz_tbl 1
;;
