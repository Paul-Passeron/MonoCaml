use crate::{
    lexer::token::{Token, TokenKind},
    parse_tree::{LongIdent, structure::Structure},
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
        self.peek_n(0)
    }

    fn peek_n(&self, n: usize) -> Option<&Token> {
        let res = self.toks.get(self.pos + n).copied();
        res
    }

    fn advance(&mut self) -> Option<&Token> {
        let tok = self.toks.get(self.pos)?;
        self.pos += 1;
        Some(tok)
    }

    fn span(&self) -> Span {
        if let Some(t) = self.toks.get(0.max(self.pos - 1)) {
            t.span
        } else {
            self.toks.last().unwrap().span
        }
    }

    fn loc(&self) -> Loc {
        if let Some(t) = self.peek() {
            t.span
        } else {
            self.toks.last().unwrap().span
        }
        .split()
        .0
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

    fn at_n(&self, n: usize, kind: TokenKind) -> bool {
        self.peek_n(n).map_or_else(|| false, |x| x.kind == kind)
    }

    fn is_done(&self) -> bool {
        self.peek().is_none()
    }

    #[allow(unused)]
    fn try_parse<T>(
        &mut self,
        funs: Vec<Box<dyn FnMut(&mut Parser<'a>) -> ParseRes<T>>>,
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
        Err(error.unwrap())
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

    fn parse_long_ident(&mut self) -> ParseRes<LongIdent> {
        // let start = self.loc();
        match self.peek().map(|x| x.kind.clone()) {
            Some(TokenKind::Ident(s)) => {
                self.advance();
                Ok(LongIdent::Ident(s))
            }
            _ => Err(ParseError::todo("other long idents", self.loc())),
        }
    }

    fn at_constructor(&mut self) -> bool {
        match self.peek_parse(Self::parse_long_ident) {
            Some(ident) => ident.last_symbol().is_constructor(),
            None => false,
        }
    }

    pub fn parse_program(&mut self) -> ParseRes<Structure> {
        self.parse_structure()
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Assoc {
    Left,
    Right,
}

impl TokenKind {
    pub fn assoc(&self) -> Option<(u8, Assoc)> {
        match self {
            TokenKind::Semi => Some((1, Assoc::Left)),
            TokenKind::Comma => Some((2, Assoc::Left)),
            TokenKind::LOr => Some((3, Assoc::Right)),
            TokenKind::LAnd => Some((4, Assoc::Right)),
            TokenKind::Eq
            | TokenKind::NEq
            | TokenKind::LT
            | TokenKind::GT
            | TokenKind::GEq
            | TokenKind::LEq => Some((5, Assoc::Left)),
            TokenKind::Cons => Some((6, Assoc::Right)),
            TokenKind::Plus | TokenKind::Minus => Some((7, Assoc::Left)),
            TokenKind::Star | TokenKind::Div => Some((8, Assoc::Left)),
            TokenKind::Op(_) => todo!("Custom operators"),
            _ => None,
        }
    }
}
