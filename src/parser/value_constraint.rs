use crate::{
    parse_tree::expression::ValueConstraint,
    parser::{Parser, error::ParseRes},
};

impl<'a> Parser<'a> {
    pub fn parse_value_constraint(&mut self) -> ParseRes<ValueConstraint> {
        todo!()
    }
}
