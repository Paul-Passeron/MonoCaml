use crate::{
    lexer::token::TokenKind,
    parse_tree::{ArgLabel, LongIdent, type_expr::TypeExpr, type_expr::TypeExprDesc},
    parser::{
        Parser,
        error::{ParseError, ParseRes},
    },
};

impl<'a> Parser<'a> {
    pub(super) fn parse_type_expr(&mut self) -> ParseRes<TypeExpr> {
        self.parse_type_expr_arrow()
    }

    fn parse_type_expr_arrow(&mut self) -> ParseRes<TypeExpr> {
        let start = self.loc();

        let label = if self.at_label() {
            todo!()
        } else {
            ArgLabel::NoLabel
        };

        let type_expr = self.parse_type_expr_tuple()?;

        if self.at(TokenKind::Arrow) {
            self.advance();
            let rhs = self.parse_type_expr_arrow()?;
            let end = self.span().split().1;
            let span = start.span(&end);
            let desc = TypeExprDesc::Arrow(label, Box::new(type_expr), Box::new(rhs));
            Ok(TypeExpr::new(desc, span))
        } else {
            Ok(type_expr)
        }
    }

    fn parse_type_expr_tuple(&mut self) -> ParseRes<TypeExpr> {
        let start = self.loc();
        let first = self.parse_type_expr_alias()?;

        if self.at(TokenKind::Star) {
            let mut elements = vec![first];
            while self.at(TokenKind::Star) {
                self.advance();
                elements.push(self.parse_type_expr_alias()?);
            }
            let end = self.span().split().1;
            let span = start.span(&end);
            let desc = TypeExprDesc::Tuple(elements);
            Ok(TypeExpr::new(desc, span))
        } else {
            Ok(first)
        }
    }

    fn parse_type_expr_alias(&mut self) -> ParseRes<TypeExpr> {
        let start = self.loc();
        let type_expr = self.parse_type_expr_constructor()?;
        let pos = self.pos;
        if let Some(TokenKind::Ident(_)) = self.peek().map(|t| &t.kind) {
            self.advance();
            if self.at_poly()
                && let Some(TokenKind::PolyTypeName(var)) = self.peek().map(|t| &t.kind)
            {
                let var_sym = *var;
                self.advance();
                let end = self.span().split().1;
                let span = start.span(&end);
                let desc = TypeExprDesc::Alias(Box::new(type_expr), var_sym);
                return Ok(TypeExpr::new(desc, span));
            }
            self.pos = pos;
        }

        Ok(type_expr)
    }

    fn parse_type_expr_constructor(&mut self) -> ParseRes<TypeExpr> {
        let start = self.loc();
        let atom = self.parse_type_expr_atom()?;
        if self.at_type_constructor() {
            let constr = self.parse_long_ident()?;
            let end = self.span().split().1;
            let span = start.span(&end);
            let desc = TypeExprDesc::Constr(vec![atom], constr);
            Ok(TypeExpr::new(desc, span))
        } else {
            Ok(atom)
        }
    }

    fn parse_type_expr_atom(&mut self) -> ParseRes<TypeExpr> {
        let start = self.loc();

        match self.peek().map(|t| &t.kind) {
            Some(TokenKind::Ident(sym)) => {
                let sym = *sym;
                self.advance();
                let end = self.span().split().1;
                let span = start.span(&end);
                let long_ident = LongIdent::Ident(sym);
                let desc = TypeExprDesc::Constr(vec![], long_ident);
                Ok(TypeExpr::new(desc, span))
            }

            Some(TokenKind::PolyTypeName(var)) => {
                let var_sym = *var;
                self.advance();
                let end = self.span().split().1;
                let span = start.span(&end);
                let desc = TypeExprDesc::Var(var_sym);
                Ok(TypeExpr::new(desc, span))
            }

            Some(TokenKind::LPar) => {
                self.advance();

                if self.at(TokenKind::RPar) {
                    self.advance();
                    let end = self.span().split().1;
                    let span = start.span(&end);

                    if self.at_type_constructor() {
                        let constr = self.parse_long_ident()?;
                        let end = self.span().split().1;
                        let span = start.span(&end);
                        let unit_span = start.span(&self.loc());
                        let unit_ty = TypeExpr::new(TypeExprDesc::Tuple(vec![]), unit_span);
                        let desc = TypeExprDesc::Constr(vec![unit_ty], constr);
                        return Ok(TypeExpr::new(desc, span));
                    }

                    let desc = TypeExprDesc::Tuple(vec![]);
                    return Ok(TypeExpr::new(desc, span));
                }

                let mut types = vec![self.parse_type_expr()?];

                while self.at(TokenKind::Comma) {
                    self.advance();
                    types.push(self.parse_type_expr()?);
                }

                self.expect(TokenKind::RPar)?;

                if self.at_type_constructor() {
                    let constr = self.parse_long_ident()?;
                    let end = self.span().split().1;
                    let span = start.span(&end);
                    let desc = TypeExprDesc::Constr(types, constr);
                    Ok(TypeExpr::new(desc, span))
                } else if types.len() == 1 {
                    Ok(types.into_iter().next().unwrap())
                } else {
                    Err(ParseError::todo(
                        "multiple types in parens without constructor",
                        self.loc(),
                    ))
                }
            }

            None => Err(ParseError::eof(self.loc())),

            _ => Err(ParseError::todo(
                format!("type expr atom: {:?}", self.peek().map(|t| &t.kind)),
                self.loc(),
            )),
        }
    }

    fn at_type_constructor(&self) -> bool {
        matches!(self.peek().map(|t| &t.kind), Some(TokenKind::Ident(_)))
    }

    fn at_label(&self) -> bool {
        // TODO
        false
    }
}
