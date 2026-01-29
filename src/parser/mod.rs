use crate::{
    lexer::token::{Token, TokenKind},
    parse_tree::structure::Structure,
    parser::error::{ParseError, ParseRes},
    source_manager::{
        FileId,
        loc::{Loc, Span},
    },
};

pub mod error;
pub mod expression;
pub mod pattern;
pub mod structure;
pub mod type_expr;
pub mod value_binding;
pub mod value_constraint;

pub struct Parser<'a> {
    pub src: FileId,
    pub toks: Vec<&'a Token>,
    pub pos: usize,
}

impl<'a> Parser<'a> {
    pub fn new(src: FileId, toks: &'a [Token]) -> Option<Self> {
        if toks.is_empty() {
            None
        } else {
            let toks = toks.iter().filter(|x| !x.is_skip()).collect();
            Some(Self { src, toks, pos: 0 })
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.toks.get(self.pos).copied()
    }

    fn advance(&mut self) -> Option<&Token> {
        let tok = self.toks.get(self.pos)?;
        self.pos += 1;
        Some(tok)
    }

    fn span(&self) -> Span {
        if let Some(t) = self.peek() {
            t.span
        } else {
            self.toks.last().unwrap().span
        }
    }

    fn loc(&self) -> Loc {
        self.span().split().0
    }

    fn expect(&mut self, kind: TokenKind) -> Result<&Token, ParseError> {
        match self.peek() {
            Some(tok) if tok.kind == kind => Ok(self.advance().unwrap()),
            Some(tok) => Err(ParseError::unexpected(tok.clone(), kind)),
            None => Err(ParseError::eof(self.loc())),
        }
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.peek().map_or_else(|| false, |x| x.kind == kind)
    }

    #[allow(unused)]
    fn try_parse<T>(
        &mut self,
        funs: Vec<Box<dyn FnMut(&mut Parser) -> ParseRes<T>>>,
    ) -> ParseRes<T> {
        assert!(!funs.is_empty());
        let mut error = None;
        let mut last_len = 0;
        for mut f in funs {
            let point = self.pos;
            match f(self) {
                Ok(res) => return Ok(res),
                Err(x) => {
                    let len = self.pos - point;
                    if len >= last_len {
                        error = Some(x);
                        last_len = len;
                    }
                }
            }
        }
        Err(error.unwrap_or(ParseError::eof(self.loc())))
    }

    fn peek_parse<F, T>(&mut self, f: F) -> Option<T>
    where
        F: FnMut(&mut Self) -> ParseRes<T>,
    {
        let anchor = self.pos;
        let mut f = f;
        let res = f(self).ok();
        self.pos = anchor;
        res
    }

    pub fn parse_program(&mut self) -> ParseRes<Structure> {
        self.parse_structure()
    }
}
