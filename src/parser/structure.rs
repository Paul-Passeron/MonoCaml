use crate::{
    lexer::token::TokenKind,
    parse_tree::structure::{Structure, StructureItem, StructureItemDesc},
    parser::{ParseRes, Parser},
};

impl<'a> Parser<'a> {
    fn parse_eval(&mut self) -> ParseRes<StructureItem> {
        let start = self.loc();
        let e = self.parse_expression()?;
        let end = self.span().split().1;
        let span = start.span(&end);
        let desc = StructureItemDesc::Eval(e);
        Ok(StructureItem::new(desc, span))
    }

    fn parse_value(&mut self) -> ParseRes<StructureItem> {
        let start = self.loc();
        let (rec_flag, bindings) = self.parse_let_bindings()?;
        let end = self.span().split().1;
        let span = start.span(&end);
        let desc = StructureItemDesc::Value(rec_flag, bindings);
        Ok(StructureItem::new(desc, span))
    }

    fn parse_type_decl_as_structure_item(&mut self) -> ParseRes<StructureItem> {
        let start = self.loc();
        self.expect(TokenKind::Type)?;
        let mut decls = vec![self.parse_type_decl()?];
        while self.at(TokenKind::And) {
            self.advance();
            decls.push(self.parse_type_decl()?);
        }
        let end = self.span().split().1;
        let span = start.span(&end);
        Ok(StructureItem::new(StructureItemDesc::Type(decls), span))
    }

    pub(super) fn parse_structure(&mut self) -> ParseRes<Structure> {
        let mut items = vec![];
        while self.peek().is_some() {
            if self.at(TokenKind::Type) {
                items.push(self.parse_type_decl_as_structure_item()?)
            } else {
                let pos = self.pos;

                let eval = self.parse_eval();
                if eval.is_ok() {
                    items.push(eval.unwrap());
                    continue;
                }
                self.pos = pos;
                items.push(self.parse_value()?);
            }
        }
        Ok(items)
    }
}
