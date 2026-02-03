use crate::{
    lexer::token::TokenKind,
    parse_tree::expression::ValueConstraint,
    parser::{Parser, error::ParseRes},
};

impl<'a> Parser<'a> {
    pub fn parse_value_constraint(&mut self) -> ParseRes<ValueConstraint> {
        self.expect(TokenKind::Colon)?;
        let typ = self.parse_type_expr()?;
        Ok(ValueConstraint {
            locally_abstract_types: vec![],
            typ,
        })
    }
}
