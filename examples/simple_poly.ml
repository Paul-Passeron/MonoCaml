let rec map f l = match l with
    | [] -> []
    | hd :: tl -> (f hd) :: map f tl

let mapped = map (fun x -> x + 1)
let mapped_list = mapped [1; 2; 3]

let helloed_lst1 = map (fun x -> "hello") ["a"; "b"; "c"]
let helloed_lst2 = map (fun x -> "hello") [1; 2; 3]
let final = map (fun x -> 2) (map (fun x -> "hello") [1; 2; 3])
