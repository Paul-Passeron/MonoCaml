use crate::{
    lexer::token::{Token, TokenKind},
    source_manager::loc::{Loc, LocKind},
};

#[derive(Debug)]
pub enum ParseErrorKind {
    EOF,
    Todo(String),
    Unexpected { got: TokenKind, expected: TokenKind },
}

#[derive(Debug)]
pub struct ParseError {
    pub loc: LocKind,
    pub kind: ParseErrorKind,
}

impl ParseError {
    pub fn unexpected(tok: Token, expected: TokenKind) -> Self {
        Self {
            loc: tok.span.into(),
            kind: ParseErrorKind::Unexpected {
                expected,
                got: tok.kind,
            },
        }
    }

    pub fn eof(l: Loc) -> Self {
        Self {
            loc: l.into(),
            kind: ParseErrorKind::EOF,
        }
    }

    pub fn todo<S: ToString>(s: S, l: Loc) -> Self {
        Self {
            loc: l.into(),
            kind: ParseErrorKind::Todo(s.to_string()),
        }
    }
}

pub type ParseRes<T> = Result<T, ParseError>;
