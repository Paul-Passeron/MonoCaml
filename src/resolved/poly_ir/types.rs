use std::fmt::Debug;
use std::hash::Hash;

use crate::{lexer::interner::Symbol, resolved::uniqueness, source_manager::loc::Span};

pub type TypeDefId = uniqueness::Uniklon<uniqueness::TypeDefMarker>;
pub type TypeVarId = uniqueness::Uniklon<uniqueness::TypeVarMarker>;

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
    Var(TypeVarId),
    Con {
        name: Symbol, // Will be a Path
        args: Vec<Type>,
    },
    Arrow(Box<Type>, Box<Type>),
    Tuple(Vec<Type>), // unit is Tuple(vec![])
    Record {
        fields: Vec<(Symbol, Type)>, // is it needed ?
    },
    Hole,  // To be inferred
    Error, // Error recovery
}
