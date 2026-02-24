(*
    If a type variable is used in a parent scope, then the same type variable refers to the same type.
    However, if they qre used in other, non-overlapping scopes, they do not necessarily ref to the same type.
*)

(* let outer (x: 'a) =
    let inner (y: 'a -> 'b) =
        y x in
    inner (fun (x: int) -> 5);
    inner (fun (z: string) -> "hey") *)

let first_fun x = failwith "todo 1"
let second_fun x = failwith "todo 2"

let _ =
    let _f = first_fun 5 in
    let _f2 = second_fun "hello" in
    let _f3 = second_fun (fun x -> 5) in ()
