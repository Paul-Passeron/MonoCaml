# Custom Types

## How to represent them ?

Recursive types (lists for instance) are represented as pointers to struct.
The struct has two fields:

- The discriminant (Which constructor ?)
- The union of all constructor arguments.

### Example: integer linked list:

```ocaml
type int_list =
  | Nil
  | Cons of int * int_list
```

The discriminant is an integer and would be 0 for Nil, 1 for Cons (in order of definition).
It is possible to choose a char rather than an int for struct packing concerns. This is an area to explore.

The second field would be the union of a single struct (because only one constructor has arguments).
The representation in C would be the following:

```C
// All type defs are at the start of the file to allow for mutually recursive types
typedef struct int_list int_list;

struct int_list {
    int _0;
    union {
    struct {
        int _0;
        int_list *_1;
    } Cons;
    } _1;
};
```

### Example: Arithmetic operations

```ocaml
type expr =
  | Int of int
  | Var of string
  | Add of expr * expr
  | Mul of expr * expr
  | Iff of expr * expr * expr
```

in C:

```C
typedef struct expr expr;

struct expr {
    int _0;
    union {
    struct { int _0; } Int;
    struct { char* _0; } Var;
    struct {
        expr* _0;
        expr* _1;
    } Add;
    struct {
        expr* _0;
        expr* _1;
    } Mul;
    struct {
        expr* _0;
        expr* _1;
        expr* _2;
    } Iff;
    } _1;
};
```

## Memory Management

Note: We should not allow cyclic references. For the moment, this is enforced by the fact that everything
is immutable.

We have three memory management primitives (used for closures for the moment):
- Register (Adds a pointer in the reference counting hashmap)
- Borrow (Increments the reference count associated to the pointer)
- Drop (Decrements the reference count associated to the pointer and frees the object if the count is zero)

When talking about borrowing and dropping objects, we refer to calling a ```borrow_{type}``` function that recursively borrows the object and ```drop_{type}``` that recursively drops the object.

```C
void borrow_int_list(int_list* lst) {
    borrow_object(lst);
    if (lst->_0 == 0) {
        // Nil do not do anything
    } else if (lst->_0 == 1) {
        borrow_int_list(lst->_1._1);
    } else {
        exit(1);
    }
}
```

```C
void drop_int_list(int_list* lst) {
    drop_object(lst);
    if (lst->_0 == 0) {
        // Nil do not do anything
    } else if (lst->_0 == 1) {
        drop_int_list(lst->_1._1);
    } else {
        exit(1);
    }
}
```

Note: These functions should be forward declared to allow for borrowing and dropping mutually recursive types.

These should be all we need.
When calling a function or a constructor with an object as argument (a closure or a rec type), we need to borrow it at the start of the function.
Then:
  - If in a regular function, it should always be dropped in all control paths
  - If in a constructor: We do not drop it as it is used by the newly built object.

A constructor should register its result.

We can introduce a new function in the runtime:

```C
int is_unique(void *ptr);
```
That returns 1 if the reference count is 1, and 0 otherwise.
This can be helpful for optimizations purposes and reusing constructors.


### How to allocate objects ?
For the moment, malloc and free will work great but we'll move on to growable arena allocators when possible.

### When to drop objects ?
When pattern matching an object:
- For the moment, extract the constructors' args and drop it
- If one of the field of the constructor is not used it, drop it as well.


### Example:
```ocaml
let rec add l = match l with
  | Nil -> 0
  | Cons(h, t) -> h + add t
```

This should borrow then drop all fields so the list would remain unchanged

On the other hand,
```ocaml
let hd l = match l with
  | Nil -> failwith "hd of empty int_list"
  | Cons (h, t) -> h
```

Let's ignore the `Nil` case because it fails.
- At the start of the function, we borrow `l`.
- When pattern matching `l`, we drop it and `t` is not used so it is dropped as well

Note: The extra cost of this is pretty big and we should be very wary of it.
The uniqueness property will be pretty important in decreasing the reference counting overhead.
Constant folding, function inlining and various optimization thechniques will be important as well.
