use crate::source_manager::loc::Span;

#[derive(Debug)]
pub struct TypedNode<T, Ty> {
    pub node: T,
    pub ty: Ty,
    pub span: Span,
}

pub type Spanned<T> = TypedNode<T, ()>;

impl<T, Ty> TypedNode<T, Ty> {
    pub fn new(node: T, span: Span, ty: Ty) -> Self {
        Self { node, ty, span }
    }

    #[inline(always)]
    pub fn as_ref(&self) -> TypedNode<&T, &Ty> {
        TypedNode {
            node: &self.node,
            ty: &self.ty,
            span: self.span,
        }
    }

    // pub fn map<U>(self, f: impl FnOnce(T, Ty) -> U) -> TypedNode<U> {
    //     TypedNode {
    //         node: f(self.node, self.ty),
    //         span: self.span,
    //     }
    // }
}

// impl Span {
//     pub fn spanned<T>(&self, node: T) -> TypedNode<T> {
//         TypedNode::new(node, *self)
//     }
// }
