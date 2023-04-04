module String = struct
  include String

  (* reverse string *)
  let rev s =
    s |> String.to_seq |> List.of_seq |> List.rev |> List.to_seq
    |> String.of_seq
end
