use crate::{lexer::interner::Symbol, source_manager::loc::Span};

#[derive(Debug)]
pub enum ResolutionError {
    UnboundValue {
        name: Symbol,
        span: Span,
    },
    UnboundType {
        name: Symbol,
        span: Span,
    },
    UnboundTypeParam {
        name: Symbol,
        span: Span,
    },
    ArityMismatch {
        type_name: Symbol,
        expected: usize,
        got: usize,
        span: Span,
    },
    DuplicateConstructor {
        name: Symbol,
        first: Span,
        second: Span,
    },
}

pub type Res<T> = Result<T, ResolutionError>;
