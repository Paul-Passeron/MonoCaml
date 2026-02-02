(* Simple type expressions *)
type int_type = int
type bool_type = bool

(* Type with type variable *)
type 'a identity = 'a

(* Arrow types *)
type int_to_int = int -> int
type binary_op = int -> int -> int

(* Tuple types *)
type pair = int * bool
type triple = int * bool * string

(* Parameterized types *)
type int_list = int list
type pair_option = (int * bool) option

(* Variant types *)
type color = Red | Green | Blue
type option_t = None | Some of int
type tree = Leaf | Node of int * tree * tree

type other_cons = Cons of {
    foo: int;
    bar: baz foobar;
}
