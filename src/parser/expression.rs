use crate::{
    lexer::token::TokenKind,
    parse_tree::{
        LongIdent,
        expression::{Constant, Expression, ExpressionDesc, RecFlag},
    },
    parser::{
        Parser,
        error::{ParseError, ParseRes},
    },
};

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Assoc {
    Left,
    Right,
}

impl TokenKind {
    pub fn assoc(&self) -> Option<(u8, Assoc)> {
        match self {
            TokenKind::LOr => Some((1, Assoc::Right)),
            TokenKind::LAnd => Some((2, Assoc::Right)),
            TokenKind::Eq
            | TokenKind::NEq
            | TokenKind::LT
            | TokenKind::GT
            | TokenKind::GEq
            | TokenKind::LEq => Some((3, Assoc::Left)),
            TokenKind::Plus | TokenKind::Minus => Some((4, Assoc::Left)),
            TokenKind::Star | TokenKind::Div => Some((5, Assoc::Left)),
            TokenKind::Op(_) => todo!("Custom operators"),
            _ => None,
        }
    }
}

impl<'a> Parser<'a> {
    pub(super) fn parse_expression(&mut self) -> ParseRes<Expression> {
        match self.peek().map(|x| &x.kind) {
            Some(TokenKind::If) => self.parse_if_then_else(),
            Some(TokenKind::Let) => self.parse_let_in(),
            _ => self.parse_binary_expression(0),
        }
    }

    fn parse_tuple_as_vec(&mut self) -> ParseRes<Vec<Expression>> {
        todo!()
    }

    fn parse_atom(&mut self) -> ParseRes<Expression> {
        let start = self.loc();
        match self.peek().map(|x| x.kind.clone()) {
            Some(TokenKind::Intlit(x)) => {
                self.advance();
                Ok(Expression::new(
                    ExpressionDesc::Constant(Constant::Int(x)),
                    start.span(&self.loc()),
                ))
            }
            Some(TokenKind::LPar) => {
                let elems = self.parse_tuple_as_vec()?;
                let end = self.span().split().1;
                let span = start.span(&end);
                let desc = if elems.is_empty() {
                    ExpressionDesc::Unit
                } else if elems.len() == 1 {
                    ExpressionDesc::paren(elems.into_iter().next().unwrap())
                } else {
                    ExpressionDesc::tuple(elems)
                };
                Ok(Expression::new(desc, span))
            }
            None => Err(ParseError::eof(self.loc())),
            _ => Err(ParseError::todo("other atoms", self.loc())),
        }
    }

    fn parse_application(&mut self) -> ParseRes<Expression> {
        let start = self.loc();
        let mut expr = self.parse_atom()?;
        let mut pos = self.pos;
        loop {
            match self.parse_atom() {
                Err(_) => {
                    self.pos = pos;
                    break;
                }
                Ok(atom) => {
                    let end = self.span().split().1;
                    let span = start.span(&end);
                    let desc = ExpressionDesc::application(expr, atom);
                    expr = Expression { desc, span };
                    pos = self.pos;
                }
            }
        }

        Ok(expr)
    }

    fn parse_long_ident(&mut self) -> ParseRes<LongIdent> {
        let start = self.loc();
        match self.peek().map(|x| x.kind.clone()) {
            Some(TokenKind::Ident(s)) => {
                let end = self.span().split().1;
                let span = start.span(&end);
                Ok(LongIdent::Ident(s))
            }
            x => Err(ParseError::todo("other long idents", self.loc())),
        }
    }

    fn at_constructor(&mut self) -> bool {
        match self.peek_parse(Self::parse_long_ident) {
            Some(ident) => ident.last_symbol().is_constructor(),
            None => false,
        }
    }

    fn parse_constructor(&mut self) -> ParseRes<Expression> {
        let start = self.loc();
        let l_ident = self.parse_long_ident()?;
        let pos = self.pos;
        let args = match self.parse_atom() {
            Ok(x) => Some(x),
            Err(_) => {
                self.pos = pos;
                None
            }
        };
        let desc = ExpressionDesc::construct(l_ident, args);
        let end = self.span().split().1;
        Ok(Expression::new(desc, start.span(&end)))
    }

    fn parse_unary_expression(&mut self) -> ParseRes<Expression> {
        match self.peek().map(|x| x.kind.clone()) {
            Some(TokenKind::Minus) => {
                todo!()
            }
            Some(_) if self.at_constructor() => self.parse_constructor(),
            _ => self.parse_application(),
        }
    }

    fn parse_binary_expression(&mut self, min_prec: u8) -> ParseRes<Expression> {
        let start = self.loc();
        let mut lhs = self.parse_unary_expression()?;
        while let Some((prec, assoc)) = self
            .peek()
            .and_then(|t| t.kind.assoc())
            .filter(|(p, _)| *p >= min_prec)
        {
            let op = self.advance().unwrap().kind.clone();
            let next_min = match assoc {
                Assoc::Left => prec + 1,
                Assoc::Right => prec,
            };
            let rhs = self.parse_binary_expression(next_min)?;
            let desc = ExpressionDesc::binary_op(op.try_into().unwrap(), lhs, rhs);
            let end = self.span().split().1;
            lhs = Expression::new(desc, start.span(&end));
        }
        Ok(lhs)
    }

    fn parse_if_then_else(&mut self) -> ParseRes<Expression> {
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

    fn parse_let_in(&mut self) -> ParseRes<Expression> {
        let start = self.loc();

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

        self.expect(TokenKind::In)?;

        let expr = self.parse_expression()?;

        let end = self.span().split().1;
        let span = start.span(&end);
        let desc = ExpressionDesc::let_in(rec_flag, bindings, expr);
        Ok(Expression { desc, span })
    }
}
