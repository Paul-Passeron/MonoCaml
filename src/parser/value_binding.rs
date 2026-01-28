use crate::{
    lexer::token::TokenKind,
    parse_tree::expression::ValueBinding,
    parser::{Parser, error::ParseRes},
};

impl<'a> Parser<'a> {
    pub fn parse_value_binding(&mut self) -> ParseRes<ValueBinding> {
        let start = self.loc();
        let pat = self.parse_pattern()?;
        let constraint = if self.at(TokenKind::Colon) {
            self.advance();
            Some(self.parse_value_constraint()?)
        } else {
            None
        };
        self.expect(TokenKind::Eq)?;
        let expr = self.parse_expression()?;
        let end = self.span().split().1;
        Ok(ValueBinding::new(pat, expr, constraint, start.span(&end)))
    }
}
