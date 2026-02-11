use crate::{lexer::Lexer, session::Session};

pub struct SkipLexer<'session> {
    session: &'session mut Session,
    inner: Lexer,
}
