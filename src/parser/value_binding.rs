use crate::{
    lexer::token::TokenKind,
    parse_tree::expression::{RecFlag, ValueBinding},
    parser::{Parser, error::ParseRes},
};

impl<'a> Parser<'a> {
    pub(super) fn parse_let_bindings(&mut self) -> ParseRes<(RecFlag, Vec<ValueBinding>)> {
        self.expect(TokenKind::Let)?;

        let rec_flag = if self.at(TokenKind::Rec) {
            self.advance();
            RecFlag::Recursive
        } else {
            RecFlag::NonRecursive
        };

        let mut bindings = vec![self.parse_value_binding()?];

        while self.at(TokenKind::And) {
            self.advance();
            bindings.push(self.parse_value_binding()?);
        }

        Ok((rec_flag, bindings))
    }

    pub(super) fn parse_value_binding(&mut self) -> ParseRes<ValueBinding> {
        let start = self.loc();
        let pat = self.parse_pattern()?;
        let constraint = if self.at(TokenKind::Colon) {
            self.advance();
            Some(self.parse_value_constraint()?)
        } else {
            None
        };
        self.expect(TokenKind::Eq)?;
        println!("Here !");
        let expr = self.parse_expression()?;
        let end = self.span().split().1;
        Ok(ValueBinding::new(pat, expr, constraint, start.span(&end)))
    }
}
