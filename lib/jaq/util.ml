let concat_json (j1 : Yojson.Safe.t) (j2 : Yojson.Safe.t) =
  match (j1, j2) with
  | `Assoc a1, `Assoc a2 -> `Assoc (a1 @ a2)
  | `List l1, `List l2 -> `List (l1 @ l2)
  | _ -> failwith "Cannot concat assoc with list!"

let rec concat_jsons (js : Yojson.Safe.t list) : Yojson.Safe.t =
  match js with
  | j :: j' :: rest -> concat_json j (concat_jsons (j' :: rest))
  | j :: [] -> j
  | [] -> failwith "Cannot concat empty json list!"

let concat_strings delim ss =
  let rec aux acc delim = function
    | s :: r :: rest -> aux (acc ^ s ^ delim) delim (r :: rest)
    | [ s ] -> acc ^ s
    | [] -> ""
  in
  aux "" delim ss

let comp f g x = f (g x)

let explode s =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let revplode s =
  let rec exp i l =
    if i = String.length s then l else exp (i + 1) (s.[i] :: l)
  in
  exp 0 []
