use crate::{
    parse_tree::pattern::Pattern,
    parser::{Parser, error::ParseRes},
};

impl<'a> Parser<'a> {
    pub fn parse_pattern(&mut self) -> ParseRes<Pattern> {
        todo!()
    }
}
