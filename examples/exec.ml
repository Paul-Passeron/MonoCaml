let my_function =
    print_endline "This output should be printed";
    (fun x -> x)

let _ = print_int (my_function 5); print_endline ""
let _ = print_endline (my_function "hey")

(* This works in OCaml because OCaml is smart enough to keep the polymorphic type of my_function ('a -> 'a) and not let it be fixed by its first use even though it is not a syntactic function. *)

let my_function =
    print_endline "This output should be printed";
    let _ = ref 0 in
    (fun x -> x)

let _ = print_int (my_function 5); print_endline ""
(* let _ = print_endline (my_function "hey") (* This call does not work anymore because the ref constrains it *) *)


module M(X: sig end) = struct
    let _x = print_endline "Hello from M" in (fun x -> x)
end

module N0 = M(struct end)
module N1 = M(struct end)
module N2 = M(struct end)
module N3 = M(struct end)
