open Jaq

let () = Read.read_stdin () |> json_pp |> print_endline
