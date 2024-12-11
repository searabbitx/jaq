let now () = Timedesc.Timestamp.now ()

let time1 name f x =
  let start = now () in
  let res = f x in
  let duration = Timedesc.Timestamp.sub (now ()) start in
  let dur_str = duration |> Timedesc.Timestamp.to_float_s |> string_of_float in
  print_endline (name ^ " " ^ dur_str);
  res
