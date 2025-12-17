// use pest::iterators::Pair;
// use pest::pratt_parser::{Assoc, Op, PrattParser};
// use pest::Parser;
// use pest_derive::Parser;

// use crate::ast::{BinaryOp, Binding, Expression, Literal, Pattern};

// #[derive(Parser)]
// pub struct MonoCamlParser;

// lazy_static::lazy_static! {
//     static ref PRATT_PARSER: PrattParser<Rule> = {
//         use Assoc::*;
//         use Rule::*;

//         PrattParser::new()
//             .op(Op::infix(comma, Left))
//             .op(Op::infix(semi, Left))
//             .op(Op::infix(or_op, Right))
//             .op(Op::infix(and_op, Right))
//             .op(Op::infix(eq, Left) | Op::infix(dif, Left))
//             .op(Op::infix(cons, Right))
//             .op(Op::infix(plus, Left) | Op::infix(minus, Left))
//             .op(Op::infix(mul, Left) | Op::infix(div, Left) | Op::infix(mod_op, Left))
//     };
// }

// pub fn parse(input: &str) -> Result<Vec<Expression>, pest::error::Error<Rule>> {
//     let mut pairs = MonoCamlParser::parse(Rule::program, input)?;
//     let mut program = pairs.next().unwrap().into_inner();
//     let mut res = vec![];
//     // while !matches!(expr_pair.as_rule(), Rule::EOI) {
//     loop {
//         let expr_pair = program.next().unwrap();
//         if matches!(expr_pair.as_rule(), Rule::EOI) {
//             break;
//         }
//         res.push(parse_expr(expr_pair)?);
//     }
//     Ok(res)
// }

// fn parse_expr(pair: Pair<Rule>) -> Result<Expression, pest::error::Error<Rule>> {
//     Ok(match pair.as_rule() {
//         Rule::expr => {
//             let inner = pair.into_inner().next().unwrap();
//             parse_expr(inner)?
//         }
//         Rule::let_expr => parse_let_expr(pair)?,
//         Rule::if_expr => parse_if_expr(pair)?,
//         Rule::lambda_expr => parse_lambda_expr(pair)?,
//         Rule::infix_expr => parse_infix_expr(pair)?,
//         Rule::non_seq_expr => parse_non_seq_expr(pair)?,
//         Rule::term => parse_term(pair)?,
//         Rule::primary => parse_primary(pair)?,
//         _ => unreachable!("Unexpected rule in parse_expr: {:?}", pair.as_rule()),
//     })
// }

// fn parse_let_expr(pair: Pair<Rule>) -> Result<Expression, pest::error::Error<Rule>> {
//     let inner = pair.into_inner();

//     let mut recursive = false;
//     let mut patterns = Vec::new();
//     let mut value_expr = None;
//     let mut in_expr = None;
//     let mut first_other_span = None;

//     for p in inner {
//         match p.as_rule() {
//             Rule::let_kw => continue,
//             Rule::rec_kw => recursive = true,
//             Rule::pattern => {
//                 if patterns.len() == 1 {
//                     first_other_span = Some(p.as_span());
//                 }
//                 patterns.push(parse_pattern(p))
//             }
//             Rule::eq => continue,
//             Rule::in_kw => continue,
//             Rule::expr => {
//                 if value_expr.is_none() {
//                     value_expr = Some(parse_expr(p)?);
//                 } else {
//                     in_expr = Some(parse_expr(p)?);
//                 }
//             }
//             _ => {}
//         }
//     }

//     let main_pattern = patterns.remove(0);

//     let can_have_args = match main_pattern {
//         Pattern::Wildcard | Pattern::Symbol(_) => true,
//         _ => false,
//     };

//     if !can_have_args && !patterns.is_empty() {
//         return Err(pest::error::Error::new_from_span(
//             pest::error::ErrorVariant::CustomError {
//                 message: "Cannot have multiple patterns without arguments".to_string(),
//             },
//             first_other_span.unwrap(),
//         ));
//     }

//     // Convert multiple patterns to nested lambdas: let f x y = e becomes let f = fun x -> fun y -> e
//     let mut expr = value_expr.unwrap();
//     for param in patterns.into_iter().rev() {
//         expr = Expression::Lambda {
//             param,
//             body: Box::new(expr),
//         };
//     }

//     Ok(Expression::Let {
//         recursive,
//         binding: Binding {
//             pat: main_pattern,
//             expr: Box::new(expr),
//         },
//         in_expr: Box::new(in_expr.unwrap()),
//     })
// }

// fn parse_if_expr(pair: Pair<Rule>) -> Result<Expression, pest::error::Error<Rule>> {
//     let inner = pair.into_inner();

//     let mut exprs = Vec::new();
//     for p in inner {
//         if p.as_rule() == Rule::expr {
//             exprs.push(parse_expr(p)?);
//         }
//     }

//     Ok(Expression::If {
//         cond: Box::new(exprs[0].clone()),
//         then_branch: Box::new(exprs[1].clone()),
//         else_branch: Box::new(exprs[2].clone()),
//     })
// }

// fn parse_lambda_expr(pair: Pair<Rule>) -> Result<Expression, pest::error::Error<Rule>> {
//     let inner = pair.into_inner();

//     let mut param = None;
//     let mut body = None;

//     for p in inner {
//         match p.as_rule() {
//             Rule::fun_kw | Rule::arrow => continue,
//             Rule::pattern => param = Some(parse_pattern(p)),
//             Rule::expr => body = Some(parse_expr(p)?),
//             _ => {}
//         }
//     }

//     Ok(Expression::Lambda {
//         param: param.unwrap(),
//         body: Box::new(body.unwrap()),
//     })
// }

// fn parse_infix_expr(pair: Pair<Rule>) -> Result<Expression, pest::error::Error<Rule>> {
//     PRATT_PARSER
//         .map_primary(|primary| parse_term(primary))
//         .map_infix(|lhs, op, rhs| {
//             let op_type = match op.as_rule() {
//                 Rule::plus => BinaryOp::Add,
//                 Rule::minus => BinaryOp::Sub,
//                 Rule::mul => BinaryOp::Mul,
//                 Rule::div => BinaryOp::Div,
//                 Rule::mod_op => BinaryOp::Mod,
//                 Rule::eq => BinaryOp::Eq,
//                 Rule::dif => BinaryOp::Neq,
//                 Rule::and_op => BinaryOp::And,
//                 Rule::or_op => BinaryOp::Or,
//                 Rule::cons => BinaryOp::Cons,
//                 Rule::semi => {
//                     return Ok(Expression::Seq {
//                         fst: Box::new(lhs?),
//                         snd: Box::new(rhs?),
//                     });
//                 }
//                 Rule::comma => {
//                     let mut items = vec![lhs?];
//                     let rhs = rhs?;
//                     if let Expression::Tuple(mut tuple_items) = rhs {
//                         items.append(&mut tuple_items);
//                         return Ok(Expression::Tuple(items));
//                     } else {
//                         items.push(rhs);
//                         return Ok(Expression::Tuple(items));
//                     }
//                 }
//                 _ => unreachable!("Unexpected infix operator: {:?}", op.as_rule()),
//             };

//             Ok(Expression::BinaryOp {
//                 op: op_type,
//                 left: Box::new(lhs?),
//                 right: Box::new(rhs?),
//             })
//         })
//         .parse(pair.into_inner())
// }

// fn parse_non_seq_expr(pair: Pair<Rule>) -> Result<Expression, pest::error::Error<Rule>> {
//     PRATT_PARSER
//         .map_primary(|primary| parse_term(primary))
//         .map_infix(|lhs, op, rhs| {
//             let op_type = match op.as_rule() {
//                 Rule::plus => BinaryOp::Add,
//                 Rule::minus => BinaryOp::Sub,
//                 Rule::mul => BinaryOp::Mul,
//                 Rule::div => BinaryOp::Div,
//                 Rule::mod_op => BinaryOp::Mod,
//                 Rule::eq => BinaryOp::Eq,
//                 Rule::dif => BinaryOp::Neq,
//                 Rule::and_op => BinaryOp::And,
//                 Rule::or_op => BinaryOp::Or,
//                 Rule::cons => BinaryOp::Cons,
//                 Rule::comma => {
//                     let mut items = vec![lhs?];
//                     let rhs = rhs?;
//                     if let Expression::Tuple(mut tuple_items) = rhs {
//                         items.append(&mut tuple_items);
//                         return Ok(Expression::Tuple(items));
//                     } else {
//                         items.push(rhs);
//                         return Ok(Expression::Tuple(items));
//                     }
//                 }
//                 _ => unreachable!("Unexpected infix operator: {:?}", op.as_rule()),
//             };

//             Ok(Expression::BinaryOp {
//                 op: op_type,
//                 left: Box::new(lhs?),
//                 right: Box::new(rhs?),
//             })
//         })
//         .parse(pair.into_inner())
// }

// fn parse_term(pair: Pair<Rule>) -> Result<Expression, pest::error::Error<Rule>> {
//     let mut inner = pair.into_inner();

//     let first = inner.next().unwrap();
//     let mut expr = parse_primary(first)?;

//     for arg_pair in inner {
//         let arg = parse_primary(arg_pair)?;
//         expr = Expression::Call {
//             fun: Box::new(expr),
//             arg: Box::new(arg),
//         };
//     }

//     Ok(expr)
// }

// fn parse_primary(pair: Pair<Rule>) -> Result<Expression, pest::error::Error<Rule>> {
//     let mut inner_pairs = pair.into_inner();
//     let inner = inner_pairs.next().unwrap();

//     Ok(match inner.as_rule() {
//         Rule::literal => parse_literal(inner),
//         Rule::ident => Expression::Var(inner.as_str().to_string()),
//         Rule::list_expr => parse_list_expr(inner)?,
//         Rule::lparen => match inner_pairs.next() {
//             None => Expression::Tuple(vec![]),
//             Some(expr_pair) => {
//                 if expr_pair.as_rule() == Rule::rparen {
//                     Expression::Tuple(vec![])
//                 } else {
//                     let first_expr = parse_expr(expr_pair)?;

//                     let mut exprs = vec![first_expr];
//                     let mut has_comma = false;

//                     for p in inner_pairs {
//                         if p.as_rule() == Rule::rparen {
//                             break;
//                         } else if p.as_rule() == Rule::comma {
//                             has_comma = true;
//                         } else {
//                             exprs.push(parse_expr(p)?);
//                         }
//                     }

//                     if has_comma {
//                         Expression::Tuple(exprs)
//                     } else if exprs.len() == 1 {
//                         exprs.into_iter().next().unwrap()
//                     } else {
//                         Expression::Tuple(exprs)
//                     }
//                 }
//             }
//         },
//         _ => unreachable!("Unexpected rule in parse_primary: {:?}", inner.as_rule()),
//     })
// }

// fn parse_list_expr(pair: Pair<Rule>) -> Result<Expression, pest::error::Error<Rule>> {
//     let inner = pair.into_inner();
//     let mut exprs = Vec::new();

//     for p in inner {
//         match p.as_rule() {
//             Rule::list_item => exprs.push(parse_expr(p.into_inner().next().unwrap())?),
//             Rule::lsquare | Rule::rsquare | Rule::semi => continue,
//             _ => {}
//         }
//     }

//     Ok(Expression::List(exprs))
// }

// fn parse_literal(pair: Pair<Rule>) -> Expression {
//     let inner = pair.into_inner().next().unwrap();

//     match inner.as_rule() {
//         Rule::int_lit => {
//             let value = inner.as_str().parse::<u64>().unwrap();
//             Expression::IntLit(value)
//         }
//         Rule::float_lit => {
//             let value = inner.as_str().parse::<f64>().unwrap();
//             Expression::FloatLit(value)
//         }
//         Rule::string_lit => {
//             let s = inner.as_str();
//             let s = &s[1..s.len() - 1];
//             Expression::StrLit(unescape_string(s))
//         }
//         Rule::char_lit => {
//             let s = inner.as_str();
//             let s = &s[1..s.len() - 1];
//             let c = unescape_string(s).chars().next().unwrap_or('\0');
//             Expression::CharLit(c)
//         }
//         Rule::true_kw => Expression::BoolLit(true),
//         Rule::false_kw => Expression::BoolLit(false),
//         _ => unreachable!("Unexpected literal rule: {:?}", inner.as_rule()),
//     }
// }

// fn parse_pattern(pair: Pair<Rule>) -> Pattern {
//     let inner = pair.into_inner().next().unwrap();

//     match inner.as_rule() {
//         Rule::wildcard_pattern => Pattern::Wildcard,
//         Rule::unit_pattern => Pattern::Tuple(vec![]),
//         Rule::symbol_pattern => {
//             let ident = inner.into_inner().next().unwrap();
//             Pattern::Symbol(ident.as_str().to_string())
//         }
//         Rule::dict_pattern => parse_dict_pattern(inner),
//         Rule::tuple_pattern => parse_tuple_pattern(inner),
//         Rule::list_pattern => parse_list_pattern(inner),
//         Rule::literal_pattern => parse_literal_pattern(inner),
//         _ => unreachable!("Unexpected pattern rule: {:?}", inner.as_rule()),
//     }
// }

// fn parse_dict_pattern(pair: Pair<Rule>) -> Pattern {
//     let mut fields = Vec::new();

//     for field_pair in pair.into_inner() {
//         if field_pair.as_rule() == Rule::dict_field {
//             let mut field_inner = field_pair.into_inner();
//             let key = field_inner.next().unwrap().as_str().to_string();
//             let pattern = parse_pattern(field_inner.next().unwrap());
//             fields.push((key, pattern));
//         }
//     }

//     Pattern::Dict(fields)
// }

// fn parse_tuple_pattern(pair: Pair<Rule>) -> Pattern {
//     let mut patterns = Vec::new();

//     for p in pair.into_inner() {
//         if p.as_rule() != Rule::lparen && p.as_rule() != Rule::rparen && p.as_rule() != Rule::comma
//         {
//             patterns.push(parse_pattern(p));
//         }
//     }

//     Pattern::Tuple(patterns)
// }

// fn parse_list_pattern(pair: Pair<Rule>) -> Pattern {
//     let mut patterns = Vec::new();

//     for p in pair.into_inner() {
//         if p.as_rule() != Rule::lsquare && p.as_rule() != Rule::rsquare && p.as_rule() != Rule::semi
//         {
//             patterns.push(parse_pattern(p));
//         }
//     }

//     Pattern::List(patterns)
// }

// fn parse_literal_pattern(pair: Pair<Rule>) -> Pattern {
//     let inner = pair.into_inner().next().unwrap();

//     let literal = match inner.as_rule() {
//         Rule::int_lit => {
//             let value = inner.as_str().parse::<u64>().unwrap();
//             Literal::Int(value)
//         }
//         Rule::float_lit => {
//             let value = inner.as_str().parse::<f64>().unwrap();
//             Literal::Float(value)
//         }
//         Rule::string_lit => {
//             let s = inner.as_str();
//             let s = &s[1..s.len() - 1];
//             Literal::String(unescape_string(s))
//         }
//         Rule::char_lit => {
//             let s = inner.as_str();
//             let s = &s[1..s.len() - 1];
//             let c = unescape_string(s).chars().next().unwrap_or('\0');
//             Literal::Char(c)
//         }
//         Rule::true_kw => Literal::Bool(true),
//         Rule::false_kw => Literal::Bool(false),
//         _ => unreachable!("Unexpected literal pattern rule: {:?}", inner.as_rule()),
//     };

//     Pattern::Literal(literal)
// }

// fn unescape_string(s: &str) -> String {
//     let mut result = String::new();
//     let mut chars = s.chars();

//     while let Some(c) = chars.next() {
//         if c == '\\' {
//             if let Some(next) = chars.next() {
//                 match next {
//                     'n' => result.push('\n'),
//                     't' => result.push('\t'),
//                     'r' => result.push('\r'),
//                     '\\' => result.push('\\'),
//                     '"' => result.push('"'),
//                     '\'' => result.push('\''),
//                     'b' => result.push('\x08'),
//                     'f' => result.push('\x0C'),
//                     '/' => result.push('/'),
//                     'u' => {
//                         let hex: String = chars.by_ref().take(4).collect();
//                         if let Ok(code) = u32::from_str_radix(&hex, 16) {
//                             if let Some(unicode_char) = char::from_u32(code) {
//                                 result.push(unicode_char);
//                             }
//                         }
//                     }
//                     _ => {
//                         result.push('\\');
//                         result.push(next);
//                     }
//                 }
//             }
//         } else {
//             result.push(c);
//         }
//     }

//     result
// }
