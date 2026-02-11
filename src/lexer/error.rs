use std::fmt;

use crate::{lexer::token::Token, source_manager::loc::Loc};

pub enum LexingError {
    EOF,
    UnexpectedChar(Loc),
    UnmatchedCommentStart(Loc),
    NotAnOperator(Loc),
    UnterminatedString(Loc),
    KeywordInPolyTypeName(Token),
    InvalidCharLiteral(Loc),
    EmptyCharLiteral(Loc),
    InvalidEscape(Loc, char),
    InvalidUnicodeEscape(Loc, String),
    InvalidAsciiEscape(Loc, String),
    InvalidCodePoint(Loc, u32),
    UnterminatedChar(Loc),
}

impl fmt::Display for LexingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            LexingError::EOF => write!(f, "EOF"),
            LexingError::UnexpectedChar(loc) => {
                write!(f, "{}: Unexpected characted", loc)
            }
            LexingError::UnmatchedCommentStart(loc) => {
                write!(f, "{}: Unmatched comment start", loc)
            }
            LexingError::NotAnOperator(loc) => write!(f, "{}: Expected an operator", loc),
            LexingError::UnterminatedString(loc) => {
                write!(f, "{}: String literal is not terminated", loc)
            }
            LexingError::KeywordInPolyTypeName(token) => write!(
                f,
                "{}: Reserved keyword used in polymorphic type name ({})",
                token.span.split().0,
                token.kind
            ),
            LexingError::InvalidCharLiteral(loc) => write!(f, "{}: Invalid character literal", loc),
            LexingError::EmptyCharLiteral(loc) => write!(f, "{}: Empty character literal", loc),
            LexingError::InvalidEscape(loc, c) => {
                write!(
                    f,
                    "{}: Invalid escape character '{}'",
                    loc,
                    c.escape_debug()
                )
            }
            LexingError::InvalidUnicodeEscape(loc, s) => {
                write!(f, "{}: Invalid unicode escape {}", loc, s)
            }
            LexingError::InvalidAsciiEscape(loc, s) => {
                write!(f, "{}: Invalid ascii escape {}", loc, s)
            }
            LexingError::InvalidCodePoint(loc, cp) => {
                write!(f, "{}: Invalid code point {}", loc, cp)
            }
            LexingError::UnterminatedChar(loc) => {
                write!(f, "{}: Unterminated char literal", loc,)
            }
        }
    }
}

pub type LexRes = Result<Token, LexingError>;
