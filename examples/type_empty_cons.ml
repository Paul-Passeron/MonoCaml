let rec add_one l = match l with
    [] -> []
    | hd :: tl -> (hd + 1) :: add_one tl
