# MonoCaml

## THIS IS A WORK IN PROGRESS
------------ 
## Overview

For the moment, we compile a statically typed, monomorphic lambda calculus-like IR to a flat SSA IR, which is then transpiled to LLVM IR, which is then compiled.
OCaml source (or rather a small subset) is parsed. The AST will be transformed into a polymorphic IR that will then be transformed in the monomorphic IR discussed earlier.

## Memory Management
Closure environnements and objects are allocated on the heap and reference-counted by a small custom runtime written in C.

## Examples

Pseudo-Code:
```
let rev: lst -> lst =
  let aux: lst -> lst -> lst = λ (acc: lst). λ (l: lst).
    match l with
      | Nil -> acc
      | Cons(hd, tl) -> aux Cons(hd, acc) tl
    in
    aux (lst.Nil)
  in
  print_lst (rev Cons(123, Cons(456, Nil)))
```
Compiles and outputs:
```
[456, 123]
```

```
let rec fact: (int -> int) = λb: int.
  if b then
    (mul b (fact (add -1 b)))
  else
    1
in
print_int (fact 6);
print_string "\n"
```

Compiles and outputs:
```
720
```

## Some implementation details thoughts

I stumbled upon a pretty powerful pattern for writing type-safe SSA in Rust. 
The assigned values in SSA (`%1`, `%2`, ...) can be defined as a type `Var` from which we can extract a value (Here it's unique ids but it could be strings, ...)

```rust
pub trait Extract<T> {
  fn extract(&self) -> T;
}

pub struct Var(usize);
```
`Var` is not clonable or copyable.
It is constructible via a `new` method which is hidden and private so that you can't create new vars with any inner value you want on the fly. 
A generator gives out "fresh" vars for us to use. 
This means that the variable can only live at one place in memory at a time.
So how can we use it in calculations like `%2 = %0 + %1` ? 
Using a View or an Use of the variable:

```rust
pub struct Use<T, S> where T: Extract<T=S> {
  id: S,
  _ghost: PhantomData<T> 
}

impl <T, S> From<&T> for Use<T> where T: Extract<T=S> {
  fn from(value: &T) -> Self {
    Self {
      id: value.extract(),
      _ghost: PhantomData,
    }
  }
}
```

This type is not "creatable" with a `new` or `default` method but only via the type it views into. 

This models parts of the SSA constraints fairly well I believe.
