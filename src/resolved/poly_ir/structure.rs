use crate::{
    resolved::poly_ir::expr::{Expr, LetBinding},
    source_manager::loc::Span,
};

pub enum PolyStructKind {
    Eval(Expr),
    Decl {
        rec: bool,
        bindings: Vec<LetBinding>,
    },
}

pub struct PolyStruct {
    pub kind: PolyStructKind,
    pub span: Span,
}
