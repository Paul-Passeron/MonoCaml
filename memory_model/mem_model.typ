#import "@preview/codelst:2.0.2": sourcecode

// #set page(
//   paper: "a4"
// )

#show title: set align(center)
#set par(
  justify: true
)

#set document(title: [MonoCaml's Memory Model])

#title[MonoCaml's Memory Model]
#align(center)[
   Passeron Paul \
   #link("mailto:paul.passeron.pro@gmail.com")
 ]

= Introduction

 The language used to describe MonoCaml is a strongly-typed lambda calculus, with no polymorphism.

 == Types

 A type can be:
- A primitive type (`int`, `string`, `float`)
- A tuple type, that is an aggregate of 0 or more types.
- An enum type with at least one constructor, taking 0 or 1 argument.
- A function type, that takes a parameter type and a result type.

The enum types can be recursive and two or more types can be mutually recursives.

=== Example: The integer linked list

#sourcecode[```ocaml
type int_list =
  | Nil
  | Cons of int * int_list
```]
Here, the constructors are `Nil`, that takes no argument and `Cons` that takes one argument: a tuple of an `int` and an `int_list`

== Constructs
As in OCaml, its main unit is the expression. Every construct is an expression.
Since this language is an intermediate target in the compilation process, it is pretty bare-bones (no modules, no functors, ...).
It has however a few very important constructs:
- `let-in` bindings that can be recursive, binding a symbol to a value and using this binding in the `in` expression. It is possible to have mutually recursive bindings.
- $lambda$ functions (exactly one typed argument).
- `match` expression to match one or more patterns over a specific value. Patterns that can be matched are unbound symbols, tuples of pattern, type constructors, strings and number literals.
- `if-then-else` construct that takes a condition expression of type `int` that is either 0 \~ `false` or `true`. The `then` and `else` branches are both non-optional and must be of the same type.
- Function application. A value of type `t` applied to a value of type `(a -> t)`.
- A native function (example: `print_string` or `random_int`).
- a tuple, an aggregate of 0 or more values.
- an integer literal.
- a string literal.
- a float literal.

All symbols are unique in this intermediate language. This means that any problem of scope and shadowing has been solved *prior* to the conversion to this IR.

#pagebreak()

=== Example: Simple list reversal
#sourcecode[```ocaml
let (reverse: int_list -> int_list) =
  let (aux: int_list -> int_list -> int_list) =
    fun (acc: int_list) -> fun (l: int_list) ->
      match l with
        | Nil => acc
        | Cons(hd, tl) -> aux (Cons(hd, acc)) tl
  in aux Nil
```]

= Memory Model

== How are types represented ?
In order to easily convey what we mean, the language used to describe the types will be C. For other backend or targets, implementation details may vary but the spirit stays the same.

=== Primitive Types
- `int` is represented as a 64-bit signed integer `int64_t`
- `string` is represented as a pointer to a null-terminated string `char *`.
- `float` is represented as a 64-bit floating point number `double`.

=== Tuples
Tuples are represented as structs, very straightforwardly.

The $i$th element of a tuple corresponds to the $i$th field of the struct.

*Ex:*
`(int, string, (float, int))` is roughly translated into

#sourcecode[```c
typedef struct ty_0 ty_0;
typedef struct ty_1 ty_1;

struct ty_0 {
    double  _0;
    int64_t _1;
};

struct ty_1 {
    int64_t _0;
    char *  _1;
    ty_0    _2;
};
```]

=== Enum types
We call recursive any type that is directly or indirectly self-referential.

*Non-recursive enums*
Non-recursive enums are represented as tagged unions struct, with the tag being an `unsigned char`.

*Ex:* The type
#sourcecode[```ocaml
type value =
  | Int of int
  | String of string
```]
is compiled into

#sourcecode[```c
typedef union un_0 un_0;
typedef struct ty_0 ty_0;
union un_0 {
    int64_t _0;
    char *  _1;
};

struct ty_0 {
  u8_t  _0; // The tag
  un_0  _1; // The union of constructor args
};
```]

*What about constructor with no arguments ?*
- When there are other constructors that have args, they are represented as a field in the union of type `unsigned char`. This will have no impact on the generated assembly as it is never accessed and has no alignement constraint.
- When there are only constructors with no arguments, the union is discarded entirely and the type is a struct with a single `tag` field.


*Recursive enums*
They are the exact same as non-recusrive enums except that they are boxed and allocated on the heap.

*Ex:* The `int_list` type

#sourcecode[```ocaml
type int_list =
  | Nil
  | Cons of int * int_list
```]
is compiled into

#sourcecode[```c
typedef union un_0 un_0;
typedef struct ty_0 ty_0;
typedef struct ty_1 ty_1;

union un_0 {
  u8_t   _0; // Nil
  ty_0 * _1; // Cons
};

// Struct representing the argument of the Cons constructor
struct ty_0 {
    int64_t _0; // first element of the tuple
    ty_0 *  _1; // Boxed, the second element of the tuple (the tail of the cons)
};

struct ty_1 {
  u8_t _0; // Tag (0: Nil, 1: Cons)
  un_0 _1; // The union of constructor args
};

// ty_1 is almost never used by itself, a pointer is always what is passed around (ty_1 *)
```]

== Reference counting
All heap-allocated values are reference-counted.
There are three primitives in the runtime:
- *`void register_object(void *ptr);`*: Creates an entry in the runtime for its reference count if ptr is not NULL (otherwise doesn't do anything).
- *`void retain_object(void *ptr);`*: Increments the ref count for the entry corresponding to ptr (does nothing if NULL, fails if invalid ptr).
- *`void release_object(void *ptr);`*: Decrements the ref count and frees the object if the count has reached 0.

*Note:* These functions are used at the lowest level and are completely type agnostic. The `release_object` function does not recursively release the children of the object. This kind of behaviour is managed by wrapper functions created for each type needing it.

== Ownership and Values
Every value has exactly one owner at any point in time.

Ownership can be transferred (moved) or shared (using `retain_object`).

When an owner relinquishes a value without transferring it, the value is released.

== Value Lifecycle
=== Allocation
Constructors allocate memory and initialize a value with a refcount = 1. Constructors consume their arguments (take ownership).

=== Sharing
To share a value with a constructor or function while retaining access, the caller must *retain* before the call.

=== Destruction
In the specially crafted *release* functions for each type needing it, when the refcount of the released object reaches zero, we recursively *release* all owned children and free the memory of the current object.

== Weak references
Weak references do not contribute to the reference count and are used to break reference cycles. Their validity should be assured by the structure of the program.

== Compiler-inserted operations
The compiler inserts *retain* and *release* calls automatically based on variable liveness.

If the current point is the last use of a variable, the ownership is transferred (no retain needed). Otherwise, retain is called.

Exiting a scope, release is inserted for all owned variables still live.

== Possible Optimizations:
Elision of retain/retain pairs when analysis ensures they are redundant.

In place mutation via uniqueness checking (is ref count equal to one). This also lends itself to type reuse (If we must allocate a new object of this type / or even of the same size then we can do so inline, just modifying the underlying data).
