open Jaq

type args = { filter : string option; file : string option }

let args =
  match Array.length Sys.argv with
  | 1 -> { filter = None; file = None }
  | 2 -> { filter = Some Sys.argv.(1); file = None }
  | 3 -> { filter = Some Sys.argv.(1); file = Some Sys.argv.(2) }
  | _ -> failwith "Incorrect number of arguments"

let filter f j = match f with None | Some "" -> j | Some f -> Filter.exec f j

let () =
  Read.read args.file |> parse_json |> filter args.filter |> json_pp
  |> Print.print
