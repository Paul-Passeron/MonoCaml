(* let rec factorial n =
    if n = 0 then 1
    else n * factorial (n - 1)
in factorial 5 *)

let factortial_plus_m n m =
  let rec factorial n =
      if n = 0 then 1
      else n * factorial (n - 1) in
  m + factorial n
in factortial_plus_m
