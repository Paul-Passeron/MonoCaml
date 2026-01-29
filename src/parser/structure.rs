use crate::{
    parse_tree::structure::{Structure, StructureItem, StructureItemDesc},
    parser::{ParseRes, Parser},
};

impl<'a> Parser<'a> {
    pub(super) fn parse_structure(&mut self) -> ParseRes<Structure> {
        let start = self.loc();
        let e = self.parse_expression()?;
        let end = self.span().split().1;
        let span = start.span(&end);
        let desc = StructureItemDesc::Eval(e);
        let item = StructureItem::new(desc, span);
        Ok(vec![item])
    }
}
