use std::fmt;

use crate::{lexer::token::Token, session::Session, source_manager::loc::Loc};

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

pub struct LexingErrorDisplay<'a, 'b>(&'a LexingError, &'b Session);

impl<'a, 'b> fmt::Display for LexingErrorDisplay<'a, 'b> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            LexingError::EOF => write!(f, "EOF"),
            LexingError::UnexpectedChar(loc) => {
                write!(
                    f,
                    "{}: Unexpected characted",
                    loc.display(&self.1.source_manager)
                )
            }
            LexingError::UnmatchedCommentStart(loc) => write!(
                f,
                "{}: Unmatched comment start",
                loc.display(&self.1.source_manager)
            ),
            LexingError::NotAnOperator(loc) => write!(
                f,
                "{}: Expected an operator",
                loc.display(&self.1.source_manager)
            ),
            LexingError::UnterminatedString(loc) => write!(
                f,
                "{}: String literal is not terminated",
                loc.display(&self.1.source_manager)
            ),
            LexingError::KeywordInPolyTypeName(token) => write!(
                f,
                "{}: Reserved keyword used in polymorphic type name ({})",
                token.span.split().0.display(&self.1.source_manager),
                token.kind.display(&self.1)
            ),
            LexingError::InvalidCharLiteral(loc) => write!(
                f,
                "{}: Invalid character literal",
                loc.display(&self.1.source_manager)
            ),
            LexingError::EmptyCharLiteral(loc) => write!(
                f,
                "{}: Empty character literal",
                loc.display(&self.1.source_manager)
            ),
            LexingError::InvalidEscape(loc, c) => {
                write!(
                    f,
                    "{}: Invalid escape character '{}'",
                    loc.display(&self.1.source_manager),
                    c.escape_debug()
                )
            }
            LexingError::InvalidUnicodeEscape(loc, s) => write!(
                f,
                "{}: Invalid unicode escape {}",
                loc.display(&self.1.source_manager),
                s
            ),
            LexingError::InvalidAsciiEscape(loc, s) => write!(
                f,
                "{}: Invalid ascii escape {}",
                loc.display(&self.1.source_manager),
                s
            ),
            LexingError::InvalidCodePoint(loc, cp) => write!(
                f,
                "{}: Invalid code point {}",
                loc.display(&self.1.source_manager),
                cp
            ),
            LexingError::UnterminatedChar(loc) => {
                write!(
                    f,
                    "{}: Unterminated char literal",
                    loc.display(&self.1.source_manager),
                )
            }
        }
    }
}

impl LexingError {
    pub fn display<'a, 'b>(&'a self, s: &'b Session) -> LexingErrorDisplay<'a, 'b> {
        LexingErrorDisplay(self, s)
    }
}

pub type LexRes = Result<Token, LexingError>;
