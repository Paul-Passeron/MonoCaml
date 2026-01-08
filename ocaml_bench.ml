let _ = Random.self_init ()
(*
Equivalent MonoCaml IR:
let rec (a:(int -> int)) = (λ(c:int).if c then ((mul c) (a ((add -1) c))) else 1) in let rec (b:(int -> ())) = (λ(c:int).if c then let (d:int) = (a (random_int 10)) in (((print_int d); (print_string "\n")); (b ((add -1) c))) else (print_string "Done !\n")) in (b 100000)

*)
(* let rec fact n =
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
in loo 100000 *)

let random_int n = Random.int_in_range ~min:0 ~max:n

type lst =
  | Nil
  | Cons of int * lst


let print_lst =
  let rec aux first = function
    | Nil -> ()
    | Cons (hd, tl) -> if not first then print_string ", "; print_int hd; aux false tl
  in fun l -> (print_string "["; aux true l; print_endline "]")

let rev =
  (let rec aux acc l = match l with
    | Nil -> acc
    | Cons (hd, tl) -> aux (Cons(hd, acc)) tl
  in aux (Nil))

let rec random_lst n =
  if n = 0 then Nil
  else Cons((+) 100 (random_int 899), random_lst (n-1))

let rec loo n =
  if n > 0 then
    let l = random_lst 100 in
    print_lst l;
    print_lst (rev l);
    loo (n-1)

let _ = loo 1000
