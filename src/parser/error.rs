use crate::{
    lexer::token::{Token, TokenKind},
    source_manager::loc::{Loc, LocKind},
};

pub enum ParseErrorKind {
    EOF,
    Unexpected { expected: TokenKind, got: TokenKind },
}

pub struct ParseError {
    pub loc: LocKind,
    pub kind: ParseErrorKind,
}

impl ParseError {
    pub fn unexpected(tok: Token, got: TokenKind) -> Self {
        Self {
            loc: tok.span.into(),
            kind: ParseErrorKind::Unexpected {
                expected: tok.kind,
                got,
            },
        }
    }

    pub fn eof(l: Loc) -> Self {
        Self {
            loc: l.into(),
            kind: ParseErrorKind::EOF,
        }
    }
}

pub type ParseRes<T> = Result<T, ParseError>;
