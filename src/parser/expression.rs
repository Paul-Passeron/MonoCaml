use crate::{
    lexer::token::TokenKind,
    parse_tree::expression::{Case, Constant, Expression, ExpressionDesc},
    parser::{
        Assoc, Parser,
        error::{ParseError, ParseRes},
    },
};

impl<'a> Parser<'a> {
    pub(super) fn parse_expression(&mut self) -> ParseRes<Expression> {
        match self.peek().map(|x| &x.kind) {
            Some(TokenKind::If) => self.parse_if_then_else(),
            Some(TokenKind::Let) => self.parse_let_in(),
            Some(TokenKind::Match) => self.parse_match(),
            Some(TokenKind::Fun) => self.parse_fun(),
            Some(TokenKind::Function) => self.parse_function(),
            _ => self.parse_binary_expression(0),
        }
    }

    fn parse_function(&mut self) -> ParseRes<Expression> {
        let start = self.loc();
        self.expect(TokenKind::Function)?;
        if self.at(TokenKind::Pipe) {
            self.advance();
        }
        let mut cases = vec![self.parse_case()?];

        while self.at(TokenKind::Pipe) {
            self.advance();
            cases.push(self.parse_case()?)
        }

        let end = self.span().split().1;
        let span = start.span(&end);
        let desc = ExpressionDesc::Function(cases);
        Ok(Expression::new(desc, span))
    }

    fn parse_fun(&mut self) -> ParseRes<Expression> {
        self.expect(TokenKind::Fun)?;
        let mut params = vec![self.parse_pattern()?];
        while !self.is_done() && !self.at(TokenKind::Arrow) {
            params.push(self.parse_pattern()?);
        }
        self.expect(TokenKind::Arrow)?;
        let body = self.parse_expression()?;
        let end = self.span().split().1;
        Ok(params.into_iter().rev().fold(body, |acc, pat| {
            let start = pat.span.split().0;
            let desc = ExpressionDesc::fun(pat, acc);
            let span = start.span(&end);
            Expression::new(desc, span)
        }))
    }

    fn parse_case(&mut self) -> ParseRes<Case> {
        let pat = self.parse_pattern()?;
        let guard = if self.at(TokenKind::When) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };
        self.expect(TokenKind::Arrow)?;
        let expr = self.parse_expression()?;
        Ok(Case::new(pat, guard, expr))
    }

    fn parse_match(&mut self) -> ParseRes<Expression> {
        let start = self.loc();

        self.expect(TokenKind::Match)?;
        let expr = self.parse_expression()?;
        self.expect(TokenKind::With)?;
        if self.at(TokenKind::Pipe) {
            self.advance();
        }

        let mut cases = vec![self.parse_case()?];

        while self.at(TokenKind::Pipe) {
            self.advance();
            cases.push(self.parse_case()?)
        }

        let end = self.span().split().1;

        let span = start.span(&end);
        let desc = ExpressionDesc::match_with(expr, cases);
        Ok(Expression::new(desc, span))
    }

    fn parse_tuple_as_vec(&mut self) -> ParseRes<Vec<Expression>> {
        self.expect(TokenKind::LPar)?;
        let mut v = vec![];
        let e = self.parse_expression()?;

        fn flatten(v: &mut Vec<Expression>, current: Expression) {
            if matches!(&current.desc, ExpressionDesc::Product(_, _)) {
                match current.desc {
                    ExpressionDesc::Product(a, b) => {
                        flatten(v, *a);
                        v.push(*b)
                    }
                    _ => unreachable!(),
                }
            } else {
                v.push(current);
            }
        }

        flatten(&mut v, e);

        self.expect(TokenKind::RPar)?;
        Ok(v)
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
            Some(TokenKind::Strlit(s)) => {
                self.advance();
                Ok(Expression::new(
                    ExpressionDesc::Constant(Constant::String(s)),
                    start.span(&self.loc()),
                ))
            }
            Some(TokenKind::True) => {
                self.advance();
                Ok(Expression::new(
                    ExpressionDesc::Constant(Constant::Bool(true)),
                    start.span(&self.loc()),
                ))
            }
            Some(TokenKind::False) => {
                self.advance();
                Ok(Expression::new(
                    ExpressionDesc::Constant(Constant::Bool(false)),
                    start.span(&self.loc()),
                ))
            }
            Some(TokenKind::LSqr) => {
                let start = self.loc();
                self.advance();
                let mut elems = vec![];
                while !self.is_done() && !self.at(TokenKind::RSqr) {
                    elems.push(self.parse_atom()?);
                    if !self.at(TokenKind::Semi) {
                        break;
                    } else {
                        self.advance();
                    }
                }

                self.expect(TokenKind::RSqr)?;
                let end = self.span().split().1;
                let span = start.span(&end);
                let desc = ExpressionDesc::List(elems);
                Ok(Expression::new(desc, span))
            }
            Some(TokenKind::LPar) => {
                let (desc, span) = if self.at_n(1, TokenKind::RPar) {
                    self.advance();
                    self.advance();
                    let end = self.span().split().1;
                    let span = start.span(&end);
                    (ExpressionDesc::Unit, span)
                } else {
                    let pos = self.pos;
                    let elems = self.parse_tuple_as_vec()?;
                    let end = self.span().split().1;
                    let span = start.span(&end);
                    (
                        if elems.is_empty() {
                            ExpressionDesc::Unit
                        } else if elems.len() == 1 {
                            ExpressionDesc::paren(elems.into_iter().next().unwrap())
                        } else {
                            self.pos = pos;
                            self.expect(TokenKind::LPar)?;
                            let e = self.parse_expression()?;
                            self.expect(TokenKind::RPar)?;
                            ExpressionDesc::paren(e)
                        },
                        span,
                    )
                };
                Ok(Expression::new(desc, span))
            }
            Some(TokenKind::Ident(_)) => {
                let start = self.loc();
                let desc = ExpressionDesc::Ident(self.parse_long_ident()?);
                let end = self.span().split().1;
                let span = start.span(&end);
                Ok(Expression::new(desc, span))
            }
            None => Err(ParseError::eof(self.loc())),
            x => Err(ParseError::todo(format!("other atoms {x:?}",), self.loc())),
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
            Some(TokenKind::Exclam) => {
                self.advance();
                let _e = self.parse_atom()?;
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
            let desc = ExpressionDesc::binary_op(op, lhs, rhs);
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

        let (rec_flag, bindings) = self.parse_let_bindings()?;

        self.expect(TokenKind::In)?;

        let expr = self.parse_expression()?;

        let end = self.span().split().1;
        let span = start.span(&end);
        let desc = ExpressionDesc::let_in(rec_flag, bindings, expr);
        Ok(Expression { desc, span })
    }
}
