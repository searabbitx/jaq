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
