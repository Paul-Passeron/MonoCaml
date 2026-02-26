let rec map f l = match l with
    | [] -> []
    | hd :: tl -> (f hd) :: map f tl

let mapped = map (fun x -> x + 1)
let mapped_list = mapped [1; 2; 3]

let helloed_lst1 = map (fun x -> "hello") ["a"; "b"; "c"]
let helloed_lst2 = map (fun x -> "hello") [1; 2; 3]
let final = map (fun x -> 2) (map (fun x -> "hello") [1; 2; 3])

let rec filter p l = match l with
    | [] -> []
    | hd :: tl when p hd -> hd :: filter p tl
    | _ :: tl -> filter p tl

let filtered = filter
    (fun x -> x > 5)
    [1; 9; 10; 2; 3; 4; 5; 6; 7]

let no_hellos = filter
    (fun x -> x <> "hello")
    ["apple"; "banana"; "hello"; "cherry"; "hello" ]

let rec for_each f l = match l with
    | [] -> ()
    | hd :: tl -> f hd; for_each f tl

let print_vals = for_each
    (fun elem -> print_int elem; print_string "\n")
    [69; 420]

let filter_assoc = filter
    (fun (key, _) -> key > 5)
    [(1, "one"); (2, "two"); (420, "four hundred twenty"); (421, "four hundred twenty-one")]
