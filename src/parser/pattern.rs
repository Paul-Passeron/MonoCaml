use crate::{
    parse_tree::pattern::Pattern,
    parser::{
        Parser,
        error::{ParseError, ParseRes},
    },
};

impl<'a> Parser<'a> {
    pub fn parse_pattern(&mut self) -> ParseRes<Pattern> {
        Err(ParseError::todo("parse_pattern", self.loc()))
    }
}
