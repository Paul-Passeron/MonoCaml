use crate::{lexer::token::Token, parse_tree::structure::Structure, parser::error::ParseRes};

pub mod error;
pub mod expression;
pub mod structure;
pub mod type_expr;

pub struct Parser<'a> {
    pub toks: &'a [Token],
    pub pos: usize,
}

impl<'a> Parser<'a> {
    pub fn new(toks: &'a [Token]) -> Self {
        Self { toks, pos: 0 }
    }

    pub fn parse_program(&mut self) -> ParseRes<Structure> {
        self.parse_structure()
    }
}
