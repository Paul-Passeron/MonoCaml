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
pub mod structure;
pub mod type_expr;

pub struct Parser<'a> {
    pub src: FileId,
    pub toks: &'a [Token],
    pub pos: usize,
}

// #[macro_export]
// macro_rules! try_parse {
//     ($parser: expr, $($x:expr),+) => {
//         let mut v: Vec<Box<dyn FnMut(&mut Parser<'a>) -> ParseRes<_>>>  = vec![];
//         $(
//             v.push(Box::new($x));
//         )+
//         $parser.try_parse(v)
//     };
// }

impl<'a> Parser<'a> {
    pub fn new(src: FileId, toks: &'a [Token]) -> Option<Self> {
        if toks.is_empty() {
            None
        } else {
            Some(Self { src, toks, pos: 0 })
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.toks.get(self.pos)
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

    pub fn parse_program(&mut self) -> ParseRes<Structure> {
        self.parse_structure()
    }
}
