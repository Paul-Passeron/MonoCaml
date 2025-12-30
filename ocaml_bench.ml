Random.self_init ();
(*
Equivalent MonoCaml IR:
let rec (a:(int -> int)) = (λ(c:int).if c then ((mul c) (a ((add -1) c))) else 1) in let rec (b:(int -> ())) = (λ(c:int).if c then let (d:int) = (a (random_int 10)) in (((print_int d); (print_string "\n")); (b ((add -1) c))) else (print_string "Done !\n")) in (b 100000)

*)
let rec fact n =
  if n > 0 then
    n * (fact (n-1))
  else 1
in
let rec loo n =
  if n = 0 then
    print_endline "Done !"
  else
  let res = fact (Random.int_in_range ~min:0 ~max:10) in
  print_int(res);
  print_newline ();
  loo (n-1)
in loo 100000
