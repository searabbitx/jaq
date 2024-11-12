let maybe_read_line () = try Some (read_line ()) with End_of_file -> None

let read_stdin () =
  let rec loop acc =
    match maybe_read_line () with
    | Some line -> loop (line :: acc)
    | None -> acc
  in
  loop [] |> List.rev |> String.concat "\n"
