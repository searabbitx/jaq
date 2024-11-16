let read file =
  let chan =
    match file with
    | Some f -> In_channel.open_text f
    | None -> In_channel.stdin
  in
  In_channel.input_all chan
