let rec rev l =
match l with
  | [] -> []
  | h :: q -> (rev q) @ [h]
