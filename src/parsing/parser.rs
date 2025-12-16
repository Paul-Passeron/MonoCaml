use pest_derive::Parser;

use pest::{
    iterators::Pairs,
    pratt_parser::{Assoc, Op, PrattParser},
};

use crate::parsing::parsetree::{Expression, ExpressionDesc};

#[derive(Parser)]
#[grammar = "ocaml.pest"]
pub struct MonoCamlParser;

lazy_static::lazy_static! {
    static ref PRATT: PrattParser<Rule> = {
        use Assoc::*;
        use Rule::*;

        PrattParser::new()
            .op(Op::infix(cons_op, Right))  // :: is right-associative
            .op(Op::infix(tuple_op, Right)) // , is right-associative (though could be Left)
    };
}

fn parse_expression(pairs: Pairs<Rule>) -> Expression {
    PRATT
        .map_primary(|primary| {
            // Handle your primary/atom expressions
            match primary.as_rule() {
                _ => todo!(), // Rule::simple_expression => parse_simple_expression(primary),
                              // Rule::labeled_tuple_element => parse_labeled_tuple_element(primary),
                              // ...
            }
        })
        .map_infix(|lhs, op, rhs| match op.as_rule() {
            // Rule::cons_op => Expression::mk(None, None, ExpressionDesc::),
            Rule::tuple_op => todo!(),
            _ => unreachable!(),
        })
        .parse(pairs)
}
