use crate::{
    lexer::token::TokenKind,
    parse_tree::expression::{Expression, ExpressionDesc},
    parser::{Parser, error::ParseRes},
};

impl<'a> Parser<'a> {
    pub fn parse_expression(&mut self) -> ParseRes<Expression> {
        if self.at(TokenKind::If) {
            self.parse_if_then_else()
        } else {
            todo!()
        }
    }

    pub fn parse_if_then_else(&mut self) -> ParseRes<Expression> {
        let start = self.loc();
        self.expect(TokenKind::If)?;
        let cond = self.parse_expression()?;
        self.expect(TokenKind::Then)?;
        let then_expr = self.parse_expression()?;
        let else_expr = if self.at(TokenKind::Else) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };
        let end = self.span().split().1;
        let span = start.span(&end);

        let desc = ExpressionDesc::if_then_else(cond, then_expr, else_expr);
        Ok(Expression { desc, span })
    }
}
