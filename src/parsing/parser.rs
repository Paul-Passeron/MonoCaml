use lalrpop_util::lalrpop_mod;
lalrpop_mod!(grammar);

#[test]
fn grammar() {
    // assert!(grammar::TermParser::new().parse("22").is_ok());
    // assert!(grammar::TermParser::new().parse("(22)").is_ok());
    // assert!(grammar::TermParser::new().parse("((((22))))").is_ok());
    // assert!(grammar::TermParser::new().parse("((22)").is_err());
}
