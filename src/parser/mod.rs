use crate::{lexer::Lexer, source_manager::loc::LocKind};

pub mod expression;
pub mod structure;
pub mod type_expr;

pub struct Parser {
    pub lexer: Lexer,
}

pub enum ParsingErrorKind {}

pub struct ParsingError {
    pub loc: LocKind,
    pub kind: ParsingErrorKind,
}

impl<'session> Parser {
    pub fn new(lexer: Lexer) -> Self {
        Self { lexer }
    }
}
