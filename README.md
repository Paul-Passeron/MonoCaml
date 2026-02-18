# MonoCaml

![MonoCaml Logo](monocaml.svg)

## ⚠️ THIS IS A WORK IN PROGRESS

MonoCaml is an experimental compiler for a subset of OCaml, targeting LLVM IR through a series of typed intermediate representations.

## Overview

MonoCaml compiles OCaml source code through the following pipeline:

1. **Lexer** → Tokenizes OCaml source files
2. **Parser** → Produces an untyped parse tree (AST)
3. **Resolver** → Performs name resolution and produces a polymorphic IR (Poly IR)
4. **Type Inference / Checking** → Transforms Poly IR into another Poly IR with solved types
5. **Monomorphization** → Transforms polymorphic IR into monomorphic IR (Mono IR)
6. **Lowering** → Converts Mono IR to Control Flow Graph (CFG) in SSA form
7. **Code Generation** → Compiles to LLVM IR or C code

The compiler is written in Rust and leverages type-safe SSA construction patterns.

## Current Status

### Implemented Features

- **Lexing & Parsing**: Full lexer and parser for OCaml syntax
- **Type System**: Type expressions, declarations, variants, tuples, functions
- **Name Resolution**: Scoped name resolution with type parameter support
- **Pattern Matching**: Match expressions with multiple cases
- **Type Declarations**: Variant types, record types, type aliases
- **Basic Types**: `int`, `bool`, `string`, `list`, `option`
- **Functions**: Lambda expressions, function application, recursive functions
- **Control Flow**: If-then-else, match expressions
- **Memory Management**: Reference counting via custom C runtime

### In Progress

- **Type Inference**: Basic inference implemented, full Hindley-Milner inference in progress
- **Polymorphism**: Type parameter resolution and monomorphization
- **Standard Library**: Only basic built-in functions currently available

### TODO

- **Modules**: Module system has not been worked on
- **Functors**: Not implemented
- **Objects & Classes**: Not supported and certainely will never be
- **Effects/Exceptions**: Not implemented and to be researched

## Architecture

### Intermediate Representations

#### Parse Tree
Untyped AST directly from parsing OCaml source. Represents the syntactic structure.

#### Poly IR (Polymorphic IR)
Typed intermediate representation that preserves polymorphism. Includes:
- Resolved identifiers
- Type parameters and constraints
- Polymorphic type expressions
- Is suitable for before and after inference because it takes the `type` type as a parameter.

#### Mono IR (Monomorphic IR)
Lambda calculus-like IR after monomorphization. All type variables are eliminated through specialization.

#### CFG (Control Flow Graph)
Flat SSA representation with:
- Basic blocks
- Explicit control flow (goto, branch, return)
- Typed variables and instructions
- Function signatures

### Memory Management

Objects and closures are heap-allocated and managed through **reference counting** using a custom runtime written in C. The runtime provides:
- Object allocation and deallocation
- Automatic reference counting
- Memory pooling for efficiency

Runtime files are located in `runtime/`:
- `runtime.c/h` - Core runtime functions
- `pool.h` - Memory pool allocator
- `mem.h` - Memory management utilities

The memory model remains to be improved, especially concerning scope-based book-keeping and optimizations. 

### Backend

The CFG can be compiled to either:
- **LLVM IR** (via `inkwell` bindings) - primary target
- **C code** - older alternative backend for debugging

The LLVM backend links against the custom runtime to provide memory management.

## Examples

### Hello World
```ocaml
let _ = print_endline "Hello, World!"
```

### List Reversal with Pattern Matching
```ocaml
let rec rev (l : 'a list): 'a list = match l with
    | a :: b -> (rev b) @ [a]
    | [] -> []
```

### Type Declarations
```ocaml
(* Variant types *)
type color = Red | Green | Blue

(* Parameterized types *)
type 'a option = None | Some of 'a

(* Recursive types *)
type tree = Leaf | Node of int * tree * tree
```

### Polymorphic Functions
```ocaml
let identity (x: 'a) : 'a = x

let _ = print_int (identity 42)
let _ = print_endline (identity "hello")
```

### Running

The main entry point currently compiles a hardcoded file (A real cli will be made when more of the compiler is actually done):

```bash
cargo run
```

Tests compile small AST examples and verify:
- Successful compilation
- Correct execution
- Memory leak freedom (via Valgrind)

## Implementation Details

### Type-Safe SSA in Rust

We use the following pattern for type-safe SSA construction:

```rust
pub struct Var(usize);  // Not Clone or Copy
pub struct Use<T> { ... }  // Represents a "use" of a variable
```

Variables can only exist in one place at a time (not `Clone`/`Copy`), but we can create `Use<Var>` references that represent uses of the variable in instructions. This models SSA constraints at the type level:
- Each variable is defined exactly once
- Variables can be used multiple times via `Use<Var>`
- Fresh variable generation is controlled through a generator

This pattern prevents (some) SSA construction errors at compile time.
