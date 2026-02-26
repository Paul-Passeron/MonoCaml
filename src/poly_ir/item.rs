use crate::{
    lexer::interner::Symbol,
    poly_ir::{TypeId, TypeParamId, expr::ValueBinding, spanned::TypedNode, type_expr::Type},
};

pub type GenItem<Ty, M> = TypedNode<ItemNode<Ty>, M>;
pub type Item<T> = GenItem<T, ()>;

#[derive(Debug, Clone)]
pub enum ItemNode<T> {
    Value {
        recursive: bool,
        bindings: Vec<ValueBinding<T>>,
    },
    Type {
        decls: Vec<TypeDecl>,
    },
}

#[derive(Debug)]
pub struct ItemInfo {
    pub name: Symbol,
}

#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub id: TypeId,
    pub name: Symbol,
    pub params: Vec<TypeParamId>,
    pub kind: TypeDeclKind,
}

#[derive(Debug)]
pub struct TypeDeclInfo {
    pub name: Symbol,
    pub params: Vec<TypeParamId>,
}

#[derive(Debug, Clone)]
pub enum TypeDeclKind {
    Alias(Type),
    Variant(Vec<Constructor>),
}

#[derive(Debug, Clone)]
pub struct Constructor {
    pub name: Symbol,
    pub arg: Option<ConstructorArg>,
}

#[derive(Debug, Clone)]
pub enum ConstructorArg {
    Tuple(Vec<Type>),
}
