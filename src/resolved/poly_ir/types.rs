use std::fmt::Debug;
use std::hash::Hash;

use crate::{lexer::interner::Symbol, source_manager::loc::Span};

// TODO: Make it not public, maybe using Unique
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct TypeId(pub u32);

#[derive(Debug, Clone, Hash)]
pub struct Typed<T>
where
    T: Debug + Hash,
{
    this: T,
    typ: Type,
}

#[derive(Debug, Clone, Hash)]
pub enum TypeDefKind {
    Struct(Vec<Typed<Symbol>>),
    Cons(Vec<(Symbol, Vec<Type>)>),
    Alias(Type),
}

#[derive(Debug, Clone, Hash)]
pub struct TypeDef {
    name: Symbol,
    params: Vec<Symbol>,
    kind: TypeDefKind,
    span: Span,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Type {
    Specialized { decl: TypeId, args: Vec<Box<Type>> },
    Single { decl: TypeId },
}
