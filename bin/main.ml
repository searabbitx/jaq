open Jaq

let filter_str =
  match Array.length Sys.argv with
  | 1 -> None
  | 2 -> Some Sys.argv.(1)
  | _ -> failwith "Incorrect number of arguments"

let filter f j = match f with None -> j | Some f -> Filter.exec f j

let () =
  Read.read_stdin () |> parse_json |> filter filter_str |> json_pp
  |> Print.print
