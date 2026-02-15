use crate::{
    lexer::interner::Symbol,
    poly_ir::{TypeId, expr::ValueBinding, spanned::Spanned, type_expr::Type},
};

pub type Item = Spanned<ItemNode>;

#[derive(Debug)]
pub enum ItemNode {
    Value {
        recursive: bool,
        bindings: Vec<ValueBinding>,
    },
    Type {
        decls: Vec<TypeDecl>,
    },
}

#[derive(Debug)]
pub struct ItemInfo {
    pub name: Symbol,
}

#[derive(Debug)]
pub struct TypeDecl {
    pub id: TypeId,
    pub name: Symbol,
    pub params: Vec<Symbol>,
    pub kind: TypeDeclKind,
}

#[derive(Debug)]
pub struct TypeDeclInfo {
    pub name: Symbol,
    pub arity: usize,
}

#[derive(Debug)]
pub enum TypeDeclKind {
    Alias(Type),
    Variant(Vec<Constructor>),
}

#[derive(Debug)]
pub struct Constructor {
    pub name: Symbol,
    pub arg: Option<ConstructorArg>,
}

#[derive(Debug)]
pub enum ConstructorArg {
    Tuple(Vec<Type>),
}
