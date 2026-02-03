(*
    If a type variable is used in a parent scope, then the same type variable refers to the same type.
    However, if they qre used in other, non-overlapping scopes, they do not necessarily ref to the same type.
*)

(* let outer (x: 'a) =
    let inner (y: 'a -> 'b) =
        y x in
    inner (fun (x: int) -> 5);
    inner (fun (z: string) -> "hey") *)

let first_fun (x: 'a) : 'a -> 'a = failwith "todo"
let second_fun (x: 'a) : 'a -> 'b = failwith "todo"

let _ =
    let f = first_fun 5 in
    let f2 = second_fun "hello" in
    let f3 = second_fun (fun x -> 5) in ()
