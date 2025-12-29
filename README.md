# MonoCaml

## THIS IS A WORK IN PROGRESS
------------ 
## Overview

For the moment, we compile a statically typed lambda calculus-like language to a flat SSA IR, which is then transpiled to C.

## Memory Management
Closure environnements are allocated on the stack and reference-counted by a small custom runtime written in C.

## Example
Pseudo-Code:
```
print_then_return = (λ(a:int).((print_int a); a))

(print_int (((
  (λ(a:int).
    (λ(b:int).
      (λ(c:(int -> int)).
        ((add (c a)) (c b)))))
      69)
    420)
print_then_return))
```

Compiles and executes:
```
69
420
489
```

## What's interesting

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
