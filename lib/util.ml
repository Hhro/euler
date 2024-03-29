module String = struct
  include String

  (* reverse string *)
  let rev s =
    s |> String.to_seq |> List.of_seq |> List.rev |> List.to_seq |> String.of_seq
  ;;
end

module List = struct
  include List

  let repeat e n = List.init n (fun _ -> e)
  let range l u = List.init (u - l) (fun i -> l + i)
end

module IntMap = Map.Make (Int)

module IntList = struct
  type t = int list

  let sum = List.fold_left (fun acc n -> acc + n) 0
  let max = List.fold_left (fun res n -> max res n) Int.min_int
end
