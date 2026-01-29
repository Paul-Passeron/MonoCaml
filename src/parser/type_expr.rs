use crate::{
    parse_tree::type_expr::TypeExpr,
    parser::{Parser, error::ParseRes},
};

impl<'a> Parser<'a> {
    pub(super) fn parse_type_expr(&mut self) -> ParseRes<TypeExpr> {
        todo!()
    }
}
