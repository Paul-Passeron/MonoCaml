use tree_sitter::{Node, Parser, Tree};

use crate::{
    lexing::Position,
    parsing::{
        ast_helper::LId,
        asttypes::{ArgLabel, ClosedFlag, Loc, RecFlag},
        location::Location,
        longident::LongIdent,
        parsetree::{
            Case, Constant, CoreType, Expression, FunctionBody, FunctionParam, FunctionParamDesc,
            Pattern, StructureItem, TopLevelPhrase, TypeConstraint, ValueBinding, ValueConstraint,
        },
    },
};

impl Location {
    pub fn from_node<S: ToString>(node: &Node, name: S) -> Location {
        let start_pos = node.start_position();
        let name = name.to_string();
        let end_pos = node.end_position();
        let start = Position {
            fname: name.clone(),
            lnum: start_pos.row as u32,
            bol: node.start_byte(),
            cnum: start_pos.column as u32,
        };
        let end = Position {
            fname: name,
            lnum: end_pos.row as u32,
            bol: node.end_byte(),
            cnum: end_pos.column as u32,
        };
        Location::make(start, end)
    }
}

pub struct MonoCamlParser<'a, 'b> {
    pub source_name: &'a str,
    pub source: &'b str,
}

#[derive(Debug, Clone)]
pub enum ParsingError {
    Txt(String),
}

impl<'a, 'b> MonoCamlParser<'a, 'b> {
    pub fn new(source_name: &'a str, source: &'b str) -> Self {
        Self {
            source_name,
            source,
        }
    }

    fn parse_value_definition(&self, node: Node) -> Result<StructureItem, ParsingError> {
        assert_eq!(node.kind(), "value_definition");

        let is_recursive = self.is_recursive_value_definition(node);

        let mut cursor = node.walk();
        let let_bindings: Vec<Node> = node
            .children(&mut cursor)
            .filter(|n| n.kind() == "let_binding")
            .collect();

        let bindings = let_bindings
            .into_iter()
            .map(|x| self.parse_let_binding(x))
            .collect::<Result<Vec<_>, _>>()?;

        let res = StructureItem::value(
            Some(Location::from_node(&node, self.source_name)),
            if is_recursive {
                RecFlag::Recursive
            } else {
                RecFlag::Nonrecursive
            },
            bindings,
        );

        Ok(res)
    }

    fn is_recursive_value_definition(&self, value_def: Node) -> bool {
        let mut cursor = value_def.walk();
        let mut found_let = false;

        for child in value_def.children(&mut cursor) {
            match child.kind() {
                "let_operator" => {
                    // If we have a let_operator instead of 'let', it's not recursive
                    return false;
                }
                "let_binding" => {
                    // We've reached a let_binding, stop looking
                    break;
                }
                _ if !child.is_named() => {
                    let text = child.utf8_text(self.source.as_bytes()).unwrap_or("");
                    match text {
                        "let" => found_let = true,
                        "rec" if found_let => return true,
                        "and" => break, // Hit the separator, stop
                        _ => {}
                    }
                }
                _ => {} // Skip named nodes like attributes
            }
        }

        false
    }

    pub fn parse_tree_sitter(&self) -> Result<Tree, ParsingError> {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_ocaml::LANGUAGE_OCAML.into())
            .unwrap();
        parser
            .parse(self.source, None)
            .ok_or(ParsingError::Txt("Failed to parse".into()))
    }

    pub fn parse_ocaml_source(&self) -> Result<Vec<TopLevelPhrase>, ParsingError> {
        let tree = self.parse_tree_sitter()?;
        let root_node = tree.root_node();
        self.convert_node_to_ast(root_node)
    }

    fn convert_node_to_ast(&self, node: Node) -> Result<Vec<TopLevelPhrase>, ParsingError> {
        match node.kind() {
            "compilation_unit" => self.convert_compilation_unit(node),
            _ => unreachable!(),
        }
    }

    fn parse_application(&self, node: Node) -> Result<Expression, ParsingError> {
        let loc = Location::from_node(&node, self.source_name);
        let fun = self.parse_expression(node.child_by_field_name("function").unwrap())?;
        let arg = self.parse_expression(node.child_by_field_name("argument").unwrap())?;
        Ok(fun.apply(Some(loc), None, vec![(ArgLabel::NoLabel, arg)]))
    }

    fn parse_infix(&self, node: Node) -> Result<Expression, ParsingError> {
        let loc = Location::from_node(&node, self.source_name);
        let lhs = self.parse_expression(node.child_by_field_name("left").unwrap())?;
        let op_node = node.child_by_field_name("operator").unwrap();
        let op_txt = &self.source[op_node.byte_range()];
        let rhs = self.parse_expression(node.child_by_field_name("right").unwrap())?;

        match op_txt {
            "::" => {
                let tuple_expr =
                    Expression::tuple(Some(loc.clone()), None, vec![(None, lhs), (None, rhs)]);

                Ok(Expression::construct(
                    Some(loc.clone()),
                    None,
                    LongIdent::ident("Cons", loc.clone()),
                    Some(tuple_expr),
                ))
            }
            op => {
                let value =
                    Expression::ident(Some(loc.clone()), None, LongIdent::ident(op, loc.clone()));
                let args = vec![(ArgLabel::NoLabel, lhs), (ArgLabel::NoLabel, rhs)];
                Ok(value.apply(Some(loc.clone()), None, args))
            }
        }
    }

    fn parse_parameter(&self, node: Node) -> Result<FunctionParam, ParsingError> {
        let loc = Location::from_node(&node, self.source_name);
        let node = node.child(0).unwrap();
        match node.kind() {
            "typed_pattern" => {
                let pat = self.parse_pattern(node.child_by_field_name("pattern").unwrap())?;
                let ty = self.parse_type_expression(node.child_by_field_name("type").unwrap())?;

                let pat = pat.constraint(Some(loc.clone()), None, ty);

                Ok(FunctionParam {
                    desc: FunctionParamDesc::Val(ArgLabel::NoLabel, None, pat),
                    loc,
                })
            }
            _ => {
                let pattern = self.parse_pattern(node)?;
                Ok(FunctionParam {
                    desc: FunctionParamDesc::Val(ArgLabel::NoLabel, None, pattern),
                    loc,
                })
            }
        }
    }

    fn parse_fun_expression(&self, node: Node) -> Result<Expression, ParsingError> {
        let loc = Location::from_node(&node, self.source_name);
        let params = node
            .children(&mut node.walk())
            .filter(|x| x.kind() == "parameter")
            .map(|x| self.parse_parameter(x))
            .collect::<Result<Vec<_>, _>>()?;

        let constraint = node
            .children(&mut node.walk())
            .find(|x| &self.source[x.byte_range()] == ":")
            .map(|x| x.next_sibling().unwrap())
            .map(|x| self.parse_type_expression(x))
            .transpose()?
            .map(|x| TypeConstraint::Constraint(x));

        let body = FunctionBody::Body(Box::new(
            self.parse_expression(node.child_by_field_name("body").unwrap())?,
        ));

        Ok(Expression::function(
            Some(loc.clone()),
            None,
            params,
            constraint,
            body,
        ))
    }

    fn parse_if_expression(&self, node: Node) -> Result<Expression, ParsingError> {
        let loc = Location::from_node(&node, self.source_name);
        let cond = node.child_by_field_name("condition").unwrap();

        let then_clause = node
            .children(&mut node.walk())
            .find(|x| x.kind() == "then_clause")
            .unwrap();
        let else_clause = node
            .children(&mut node.walk())
            .find(|x| x.kind() == "else_clause");

        Ok(Expression::ifthenelse(
            Some(loc.clone()),
            None,
            self.parse_expression(cond)?,
            self.parse_expression(then_clause)?,
            else_clause.map(|x| self.parse_expression(x)).transpose()?,
        ))

        // let then_clause = node.children(&mut node.walk()).find(|x| x.kind() == then_clas)
    }

    fn parse_match_expression(&self, node: Node) -> Result<Expression, ParsingError> {
        let loc = Location::from_node(&node, self.source_name);
        let mut cursor = node.walk();

        let expr = self.parse_expression(node.child_by_field_name("expression").unwrap())?;

        let cases = node
            .children(&mut cursor)
            .filter_map(|x| {
                if x.kind() == "match_case" {
                    Some(self.parse_case(x))
                } else {
                    None
                }
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(expr.match_(Some(loc), None, cases))
    }

    fn parse_case(&self, node: Node) -> Result<Case, ParsingError> {
        let pattern = self.parse_pattern(node.child_by_field_name("pattern").unwrap())?;
        let guard = match node
            .child_by_field_name("guard")
            .map(|x| self.parse_expression(x))
        {
            Some(Err(x)) => Err(x),
            Some(Ok(x)) => Ok(Some(Box::new(x))),
            None => Ok(None),
        }?;
        let body = self.parse_expression(node.child_by_field_name("body").unwrap())?;

        Ok(Case {
            lhs: pattern,
            guard,
            rhs: Box::new(body),
        })
    }

    fn parse_let_in_expression(&self, node: Node) -> Result<Expression, ParsingError> {
        let StructureItem { desc, loc } = self.parse_value_definition(node.child(0).unwrap())?;
        let (rec_flag, bindings) = match desc {
            super::parsetree::StructureItemDesc::Value(rec_flag, value_bindings) => {
                (rec_flag, value_bindings)
            }
            _ => unreachable!(),
        };
        let in_expr = self.parse_expression(node.child_by_field_name("body").unwrap())?;

        Ok(Expression::let_(
            Some(loc),
            None,
            rec_flag,
            bindings,
            in_expr,
        ))
    }

    fn parse_list_expression(&self, node: Node) -> Result<Expression, ParsingError> {
        let loc = Location::from_node(&node, self.source_name);
        let mut cursor = node.walk();
        let mut expr = Expression::construct(
            Some(loc.clone()),
            None,
            LongIdent::ident("Nil", loc.clone()),
            None,
        );
        for c in node.children(&mut cursor) {
            let as_str = &self.source[c.byte_range()];
            if as_str == "[" || as_str == "]" || as_str == ";" {
                continue;
            }
            let cur = self.parse_expression(c)?;
            let as_tuple =
                Expression::tuple(Some(loc.clone()), None, vec![(None, cur), (None, expr)]);
            expr = Expression::construct(
                Some(loc.clone()),
                None,
                LongIdent::ident("Cons", loc.clone()),
                Some(as_tuple),
            )
        }

        Ok(expr)
    }

    fn parse_string(&self, node: Node) -> Result<Expression, ParsingError> {
        let loc = Location::from_node(&node, self.source_name);
        let as_str = &self.source[node.byte_range()];
        let cte = Constant::string(None, Some(loc.clone()), as_str.into());
        Ok(Expression::constant(Some(loc), None, cte))
    }

    fn parse_number(&self, node: Node) -> Result<Expression, ParsingError> {
        let loc = Location::from_node(&node, self.source_name);
        let as_str = &self.source[node.byte_range()];
        let cte = Constant::int(Some(loc.clone()), None, as_str.parse::<i64>().unwrap());
        Ok(Expression::constant(Some(loc), None, cte))
    }

    fn parse_cons_expression(&self, node: Node) -> Result<Expression, ParsingError> {
        let loc = Location::from_node(&node, self.source_name);
        let lhs = self.parse_expression(node.child_by_field_name("left").unwrap())?;
        let rhs = self.parse_expression(node.child_by_field_name("right").unwrap())?;

        let tuple_expr = Expression::tuple(Some(loc.clone()), None, vec![(None, lhs), (None, rhs)]);

        Ok(Expression::construct(
            Some(loc.clone()),
            None,
            LongIdent::ident("Cons", loc.clone()),
            Some(tuple_expr),
        ))
    }

    fn parse_expression(&self, node: Node) -> Result<Expression, ParsingError> {
        let loc = Location::from_node(&node, self.source_name);

        match node.kind() {
            "value_path" | "value_name" => {
                // Parse variable references
                let ident = self.source[node.byte_range()].to_string();
                Ok(Expression::ident(
                    Some(loc.clone()),
                    None,
                    LongIdent::ident(ident, loc),
                ))
            }
            "application_expression" => {
                // Parse function applications
                self.parse_application(node)
            }
            "infix_expression" => {
                // Parse infix operators
                self.parse_infix(node)
            }
            "fun_expression" => {
                // Parse lambda expressions
                self.parse_fun_expression(node)
            }
            "if_expression" => {
                // Parse if-then-else
                self.parse_if_expression(node)
            }
            "match_expression" => {
                // Parse pattern matching
                self.parse_match_expression(node)
            }
            "let_expression" => {
                // Parse let-in expressions
                self.parse_let_in_expression(node)
            }
            "parenthesized_expression" => {
                // Unwrap parentheses and parse inner expression
                if let Some(inner) = node.child_by_field_name("expression") {
                    self.parse_expression(inner)
                } else {
                    Err(ParsingError::Txt("Empty parenthesized expression".into()))
                }
            }
            "expression_item" => self.parse_sequence_expression(node),
            "list_expression" => self.parse_list_expression(node),
            "string" => self.parse_string(node),
            "number" => self.parse_number(node),
            "unit" => Ok(Expression::tuple(Some(loc), None, vec![])),
            "then_clause" | "else_clause" => {
                self.parse_expression(node.child_by_field_name("expression").unwrap())
            }
            "cons_expression" => self.parse_cons_expression(node),
            // Add more expression types as needed
            unknown_kind => Err(ParsingError::Txt(format!(
                "Unknown expression kind: {}",
                unknown_kind
            ))),
        }
    }

    fn parse_sequence_expression(&self, node: Node) -> Result<Expression, ParsingError> {
        let loc = Location::from_node(&node, self.source_name);
        let mut cursor = node.walk();
        let mut it = node.children(&mut cursor);
        let fst = self.parse_expression(it.next().unwrap())?;
        it.try_fold(fst, |acc, node| {
            let e = self.parse_application(node)?;
            Ok(Expression::sequence(Some(loc.clone()), None, acc, e))
        })
    }

    fn parse_value_constraint(&self, node: Node) -> Result<ValueConstraint, ParsingError> {
        // Parse type constraints (e.g., ": int" in "let x : int = 5")
        // This depends on your ValueConstraint type definition

        // Typically, a constraint node will have a type expression child
        if let Some(type_expr) = node.child_by_field_name("type") {
            let core_type = self.parse_type_expression(type_expr)?;
            Ok(ValueConstraint::Constraint {
                locally_abstract_univars: vec![],
                typ: core_type,
            })
        } else {
            Err(ParsingError::Txt("Invalid type constraint".into()))
        }
    }

    fn parse_cons_pattern(&self, node: Node) -> Result<Pattern, ParsingError> {
        let loc = Location::from_node(&node, self.source_name);

        let lhs =
            self.parse_pattern(node.child_by_field_name("left").ok_or_else(|| {
                ParsingError::Txt("Missing left-hand side in cons pattern".into())
            })?)?;

        let rhs = self.parse_pattern(node.child_by_field_name("right").ok_or_else(|| {
            ParsingError::Txt("Missing right-hand side in cons pattern".into())
        })?)?;

        let as_tuple = Pattern::tuple(
            Some(loc.clone()),
            None,
            vec![(None, lhs), (None, rhs)],
            ClosedFlag::Closed,
        );

        Ok(Pattern::construct(
            Some(loc.clone()),
            None,
            LongIdent::ident("Cons", loc),
            Some((vec![], as_tuple)),
        ))
    }

    fn parse_pattern(&self, node: Node) -> Result<Pattern, ParsingError> {
        let loc = Location::from_node(&node, self.source_name);

        match node.kind() {
            "value_pattern" | "value_name" => {
                let name = self.source[node.byte_range()].to_string();
                Ok(Pattern::var(Some(loc.clone()), None, Loc::new(name, loc)))
            }
            "typed_pattern" => {
                // Handle patterns with type annotations
                let pattern =
                    self.parse_pattern(node.child_by_field_name("pattern").ok_or_else(|| {
                        ParsingError::Txt("Missing pattern in typed pattern".into())
                    })?)?;

                if let Some(type_node) = node.child_by_field_name("pattern").unwrap().next_sibling()
                {
                    let typ = self.parse_type_expression(type_node)?;
                    Ok(pattern.constraint(Some(loc), None, typ))
                } else {
                    Ok(pattern)
                }
            }
            "tuple_pattern" => {
                // Handle tuple patterns
                let mut cursor = node.walk();
                let patterns: Vec<Pattern> = node
                    .children(&mut cursor)
                    .filter(|n| n.is_named() && n.kind() != ",")
                    .map(|n| self.parse_pattern(n))
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(Pattern::tuple(
                    Some(loc),
                    None,
                    patterns.into_iter().map(|x| (None, x)).collect(),
                    ClosedFlag::Closed,
                ))
            }
            "list_pattern" => {
                // Handle list patterns
                self.parse_list_pattern(node)
            }
            "constructor_pattern" => {
                // Handle constructor patterns
                self.parse_constructor_pattern(node)
            }
            "cons_pattern" => self.parse_cons_pattern(node),
            "unit" => Ok(Pattern::tuple(Some(loc), None, vec![], ClosedFlag::Closed)),
            // Add more pattern types as needed
            unknown_kind => Err(ParsingError::Txt(format!(
                "Unknown pattern kind: {}",
                unknown_kind
            ))),
        }
    }

    fn parse_path(&self, node: Node) -> Result<LId, ParsingError> {
        let loc = Location::from_node(&node, self.source_name);
        let start_pos = loc.start.clone();
        let mut cursor = node.walk();
        let mut atoms = node
            .children(&mut cursor)
            .filter(|x| &self.source[x.byte_range()] != ".");
        let fst = LongIdent::Ident(self.source[atoms.next().unwrap().byte_range()].to_string());
        let mut end_pos = loc.end.clone();
        let lid = atoms.try_fold(fst, |acc, node| {
            let loc = Location::from_node(&node, self.source_name);
            let ident = Loc::new(self.source[node.byte_range()].to_string(), loc.clone());
            let res = Ok(LongIdent::Dot(
                Loc::new(
                    Box::new(acc),
                    Location {
                        start: start_pos.clone(),
                        end: end_pos.clone(),
                        ghost: false,
                    },
                ),
                ident,
            ));
            end_pos = loc.end.clone();
            res
        })?;

        Ok(Loc::new(
            lid,
            Location {
                start: start_pos,
                end: end_pos,
                ghost: false,
            },
        ))
    }

    fn parse_type_expression(&self, node: Node) -> Result<CoreType, ParsingError> {
        match node.kind() {
            "type_constructor_path" => {
                let loc = Location::from_node(&node, self.source_name);
                let path = self.parse_path(node)?;
                Ok(CoreType::constr(Some(loc), None, path, vec![]))
            }
            _ => Err(ParsingError::Txt(format!(
                "Unknown type expression kind: {}",
                node.kind()
            ))),
        }
    }

    fn parse_constructor_pattern(&self, _node: Node) -> Result<Pattern, ParsingError> {
        todo!()
    }

    fn parse_list_pattern(&self, node: Node) -> Result<Pattern, ParsingError> {
        let loc = Location::from_node(&node, self.source_name);
        let mut cursor = node.walk();
        let mut pat = Pattern::construct(
            Some(loc.clone()),
            None,
            LongIdent::ident("Nil", loc.clone()),
            None,
        );
        for c in node.children(&mut cursor) {
            let as_str = &self.source[c.byte_range()];
            if as_str == "[" || as_str == "]" || as_str == ";" {
                continue;
            }
            let cur = self.parse_pattern(c)?;

            let as_tuple = Pattern::tuple(
                Some(loc.clone()),
                None,
                vec![(None, cur), (None, pat)],
                ClosedFlag::Closed,
            );
            pat = Pattern::construct(
                Some(loc.clone()),
                None,
                LongIdent::ident("Cons", loc.clone()),
                Some((vec![], as_tuple)),
            )
        }

        Ok(pat)
    }

    fn parse_let_binding(&self, node: Node) -> Result<ValueBinding, ParsingError> {
        let loc = Location::from_node(&node, self.source_name);

        assert!(node.kind() == "let_binding");
        let pattern = self.parse_pattern(node.child_by_field_name("pattern").unwrap())?;

        let expr = self.parse_expression(
            node.child_by_field_name("body")
                .ok_or_else(|| ParsingError::Txt("Missing body in let binding".into()))?,
        )?;

        // Check for optional type constraint
        let constraint = if let Some(constraint_node) = node.child_by_field_name("constraint") {
            Some(self.parse_value_constraint(constraint_node)?)
        } else {
            None
        };

        Ok(ValueBinding::mk(
            Some(loc),
            None,
            None,
            None,
            constraint,
            pattern,
            expr,
        ))
    }

    fn parse_structure_item(&self, node: Node) -> Result<StructureItem, ParsingError> {
        match node.kind() {
            "value_definition" => self.parse_value_definition(node),
            "expression_item" => self.parse_expression_item(node),
            unknown_kind => todo!("unknown_kind {unknown_kind}"),
        }
    }

    fn parse_expression_item(&self, node: Node) -> Result<StructureItem, ParsingError> {
        let loc = Location::from_node(&node, self.source_name);
        let expr = self.parse_expression(node)?;
        Ok(StructureItem::eval(Some(loc), None, expr))
    }

    fn convert_compilation_unit(&self, node: Node) -> Result<Vec<TopLevelPhrase>, ParsingError> {
        assert!(node.kind() == "compilation_unit");
        let mut cursor = node.walk();
        let res = node
            .children(&mut cursor)
            .map(|x| {
                if x.kind() == "shebang" || x.kind() == "comment" {
                    Ok(vec![])
                } else {
                    Ok(vec![self.parse_structure_item(x)?])
                }
            })
            .collect::<Result<Vec<Vec<StructureItem>>, _>>();
        res.map(|x| x.into_iter().map(|y| TopLevelPhrase::Def(y)).collect())
    }
}
