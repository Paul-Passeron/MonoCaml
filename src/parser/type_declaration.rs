use crate::{
    lexer::{interner::Symbol, token::TokenKind},
    parse_tree::type_declaration::{
        ConstructorArguments, ConstructorDeclaration, LabelDeclaration, MutableFlag,
        TypeDeclaration, TypeKind,
    },
    parser::{
        Parser,
        error::{ParseError, ParseRes},
    },
};

impl<'a> Parser<'a> {
    fn parse_label_declaration(&mut self) -> ParseRes<LabelDeclaration> {
        let loc = self.loc();
        let mutable = if self.at(TokenKind::Mutable) {
            self.advance();
            MutableFlag::Mut
        } else {
            MutableFlag::NonMut
        };
        let name = self.parse_symbol()?;
        self.expect(TokenKind::Colon)?;
        let typ = self.parse_type_expr()?;
        Ok(LabelDeclaration {
            name,
            mutable,
            typ,
            loc,
        })
    }

    fn parse_record_decl(&mut self) -> ParseRes<Vec<LabelDeclaration>> {
        self.expect(TokenKind::LBra)?;
        let mut res = vec![];
        while !self.is_done() && !self.at(TokenKind::RBra) {
            let lab_decl = self.parse_label_declaration()?;
            res.push(lab_decl);
            if !self.at(TokenKind::Semi) {
                break;
            }
            self.expect(TokenKind::Semi)?;
        }
        self.expect(TokenKind::RBra)?;
        Ok(res)
    }

    fn parse_constructor_declaration(&mut self) -> ParseRes<ConstructorDeclaration> {
        let loc = self.loc();
        let name = self.parse_symbol()?;

        let args = if self.at(TokenKind::Of) {
            self.advance();
            if self.at(TokenKind::LBra) {
                Some(ConstructorArguments::Record(self.parse_record_decl()?))
            } else {
                let te = self.parse_type_expr()?;
                Some(ConstructorArguments::TypeExpr(te))
            }
        } else {
            None
        };
        Ok(ConstructorDeclaration { name, args, loc })
    }

    fn parse_variant_decl(&mut self) -> ParseRes<Vec<ConstructorDeclaration>> {
        let mut res = vec![self.parse_constructor_declaration()?];
        while self.at(TokenKind::Pipe) {
            self.advance();
            res.push(self.parse_constructor_declaration()?);
        }
        Ok(res)
    }

    fn parse_type_kind(&mut self) -> ParseRes<TypeKind> {
        if self.at(TokenKind::LBra) {
            Ok(TypeKind::Record(self.parse_record_decl()?))
        } else if self.at_constructor() {
            Ok(TypeKind::Variant(self.parse_variant_decl()?))
        } else {
            Ok(TypeKind::Alias(self.parse_type_expr()?))
        }
    }

    fn parse_type_params(&mut self) -> ParseRes<Vec<Symbol>> {
        let mut params = vec![];
        if self.at_poly() {
            match &self.peek().ok_or_else(|| ParseError::eof(self.loc()))?.kind {
                TokenKind::PolyTypeName(x) => {
                    params.push(*x);
                    self.advance();
                }
                _ => unreachable!(),
            }
        } else if self.at(TokenKind::LPar) {
            self.advance();
            while !(self.at(TokenKind::RPar)) {
                match &self.peek().ok_or_else(|| ParseError::eof(self.loc()))?.kind {
                    TokenKind::PolyTypeName(x) => {
                        params.push(*x);
                        self.advance();
                    }
                    _ => unreachable!(),
                }
                if !self.at(TokenKind::Comma) {
                    break;
                }
                self.expect(TokenKind::Comma)?;
            }
            self.expect(TokenKind::RPar)?;
        }

        Ok(params)
    }

    pub(super) fn parse_type_decl(&mut self) -> ParseRes<TypeDeclaration> {
        let params = self.parse_type_params()?;
        let name = self.parse_symbol()?;
        self.expect(TokenKind::Eq)?;
        let kind = self.parse_type_kind()?;
        Ok(TypeDeclaration { name, params, kind })
    }
}
