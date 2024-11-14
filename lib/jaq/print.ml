let append_flush = Fun.flip ( ^ ) "\n%!"
let colorize raw = "@{<green>" ^ raw ^ "}@"

let print raw =
  let colorized = raw |> colorize |> append_flush in
  let fmt = Scanf.format_from_string colorized "" in
  Ocolor_format.printf fmt
