open Jaq

let filter f j = match f with None | Some "" -> j | Some f -> Filter.exec f j

let () =
  let args = Args.parse_args () in
  try
    Read.read args.file |> parse_json |> filter args.filter |> json_pp
    |> Print.print
  with e -> Error.handle_error e
