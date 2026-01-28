use crate::{
    lexer::{Lexer, token::Token},
    session::Session,
};

pub struct SkipLexer<'session> {
    session: &'session mut Session,
    inner: Lexer,
}

pub struct LexerIter<'session> {
    session: &'session mut Session,
    inner: Lexer,
}

impl<'session> Iterator for LexerIter<'session> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next_token(self.session).ok()
    }
}

impl<'session> Iterator for SkipLexer<'session> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while let Ok(tok) = self.inner.next_token(self.session) {
            if !tok.is_skip() {
                return Some(tok);
            }
        }

        None
    }
}

impl Lexer {
    pub fn no_skip<'session>(self, session: &'session mut Session) -> LexerIter<'session> {
        LexerIter {
            inner: self,
            session,
        }
    }
    pub fn skip<'session>(self, session: &'session mut Session) -> SkipLexer<'session> {
        SkipLexer {
            inner: self,
            session,
        }
    }
}
