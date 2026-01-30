use crate::{
    lexer::{interner::Symbol, token::TokenKind},
    parse_tree::{
        expression::Constant,
        pattern::{Pattern, PatternDesc},
    },
    parser::{
        Assoc, Parser,
        error::{ParseError, ParseRes},
    },
};

impl<'a> Parser<'a> {
    fn parse_atom_pat(&mut self) -> ParseRes<Pattern> {
        let start = self.loc();
        match self.peek().map(|x| x.kind.clone()) {
            Some(TokenKind::Intlit(x)) => {
                self.advance();
                Ok(Pattern::new(
                    PatternDesc::Constant(Constant::Int(x)),
                    start.span(&self.loc()),
                ))
            }
            Some(TokenKind::LPar) => {
                // Try tuple and if not try constraint
                let try_as_tuple = |this: &mut Self| -> ParseRes<Pattern> {
                    let start = this.loc();
                    let elems = this.parse_tuple_as_vec_pat()?;
                    let end = this.span().split().1;
                    let span = start.span(&end);
                    let desc = if elems.is_empty() {
                        PatternDesc::Unit
                    } else if elems.len() == 1 {
                        PatternDesc::paren(elems.into_iter().next().unwrap())
                    } else {
                        PatternDesc::tuple(elems)
                    };
                    Ok(Pattern::new(desc, span))
                };
                let try_as_constraint = |this: &mut Parser<'a>| -> ParseRes<Pattern> {
                    let start = this.loc();
                    this.expect(TokenKind::LPar)?;
                    let atom = this.parse_atom_pat()?;
                    this.expect(TokenKind::Colon)?;
                    let te = this.parse_type_expr()?;
                    this.expect(TokenKind::RPar)?;
                    let end = this.span().split().1;
                    let span = start.span(&end);
                    let desc = PatternDesc::constraint(atom, te);
                    Ok(Pattern::new(desc, span))
                };

                self.try_parse(vec![Box::new(try_as_tuple), Box::new(try_as_constraint)])
            }
            Some(TokenKind::Ident(Symbol::Cons(_))) => todo!(),
            Some(TokenKind::Ident(Symbol::Ident(s))) => {
                let start = self.loc();
                self.advance();
                let end = self.span().split().1;
                let span = start.span(&end);
                let desc = PatternDesc::Var(Symbol::Ident(s));
                Ok(Pattern::new(desc, span))
            }
            Some(TokenKind::LSqr) => {
                self.advance();
                let mut elems = vec![];
                while !self.is_done() && !self.at(TokenKind::RSqr) {
                    elems.push(self.parse_atom_pat()?);
                    if !self.at(TokenKind::Semi) {
                        break;
                    }
                    self.advance();
                }
                self.expect(TokenKind::RSqr)?;

                let end = self.span().split().1;
                let span = start.span(&end);
                let desc = PatternDesc::List(elems);

                Ok(Pattern::new(desc, span))
            }
            _ => Err(ParseError::todo("parse_pattern", self.loc())),
        }
    }

    fn parse_constructor_pat(&mut self) -> ParseRes<Pattern> {
        let start = self.loc();
        let l_ident = self.parse_long_ident()?;
        let pos = self.pos;
        let args = match self.parse_atom_pat() {
            Ok(x) => Some(x),
            Err(_) => {
                self.pos = pos;
                None
            }
        };
        let desc = PatternDesc::construct(l_ident, args);
        let end = self.span().split().1;
        Ok(Pattern::new(desc, start.span(&end)))
    }

    fn parse_unary_pat(&mut self) -> ParseRes<Pattern> {
        if self.at_constructor() {
            self.parse_constructor_pat()
        } else {
            self.parse_atom_pat()
        }
    }

    fn parse_binary_pat(&mut self, min_prec: u8) -> ParseRes<Pattern> {
        let start = self.loc();
        let mut lhs = self.parse_unary_pat()?;
        while let Some((prec, assoc)) = self
            .peek()
            .filter(|t| t.kind != TokenKind::Eq)
            .and_then(|t| t.kind.assoc())
            .filter(|(p, _)| *p >= min_prec)
        {
            let op = self.advance().unwrap().kind.clone();
            let next_min = match assoc {
                Assoc::Left => prec + 1,
                Assoc::Right => prec,
            };
            let rhs = self.parse_binary_pat(next_min)?;
            let desc = PatternDesc::binary_op(op.try_into().unwrap(), lhs, rhs);
            let end = self.span().split().1;
            lhs = Pattern::new(desc, start.span(&end));
        }
        Ok(lhs)
    }

    fn parse_tuple_as_vec_pat(&mut self) -> ParseRes<Vec<Pattern>> {
        self.expect(TokenKind::LPar)?;
        let mut v = vec![];
        while !self.is_done() && !self.at(TokenKind::RPar) {
            let start = self.loc();
            let mut atom = self.parse_atom_pat()?;
            if self.at(TokenKind::Colon) {
                self.advance();
                let te = self.parse_type_expr()?;
                let end = self.span().split().1;
                let span = start.span(&end);
                let desc = PatternDesc::constraint(atom, te);
                atom = Pattern::new(desc, span);
            }
            v.push(atom);
            if self.at(TokenKind::Comma) {
                self.advance();
            } else {
                break;
            }
        }
        self.expect(TokenKind::RPar)?;
        Ok(v)
    }

    pub fn parse_pattern(&mut self) -> ParseRes<Pattern> {
        self.parse_binary_pat(0)
    }
}
