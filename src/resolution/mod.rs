use crate::{
    SESSION,
    lexer::interner::Symbol,
    parse_tree::{
        self, LongIdent,
        expression::{Expression, ExpressionDesc},
        pattern::PatternDesc,
        structure::{Structure, StructureItem, StructureItemDesc},
        type_declaration::{
            ConstructorArguments, ConstructorDeclaration, TypeDeclaration, TypeKind,
        },
        type_expr::{TypeExpr, TypeExprDesc},
    },
    poly_ir::{
        TypeId, TypeParamId, ValueRef, VarMarker,
        expr::{Expr, ExprNode, MatchCase, ValueBinding},
        id::Arena,
        item::{Constructor, ConstructorArg, Item, ItemNode, TypeDecl, TypeDeclInfo, TypeDeclKind},
        pattern::{Pattern, PatternNode},
        type_expr::{Type, TypeNode, TypeVarId},
    },
    resolution::{error::Res, scope::Scope},
    source_manager::loc::Span,
};

pub mod builtins;
pub mod error;
pub mod scope;

pub struct Resolver {
    pub types: Arena<TypeDeclInfo>,
    pub vars: Arena<VarMarker>,
    scope: Scope,
    binder_depth: u32,
    type_vars: u32,
}

fn flatten_product<'a>(expr: &'a Expression) -> Vec<&'a Expression> {
    fn aux<'a>(e: &'a Expression, v: &mut Vec<&'a Expression>) {
        match &e.desc {
            ExpressionDesc::Product(x1, x2) => {
                v.push(x1);
                aux(x2, v);
            }
            _ => v.push(e),
        }
    }
    let mut v = vec![];
    aux(expr, &mut v);
    v
}

impl Resolver {
    pub fn new() -> Self {
        let mut res = Self {
            types: Arena::new(),
            vars: Arena::new(),
            scope: Scope::new(),
            binder_depth: 0,
            type_vars: 0,
        };
        res.add_builtins();
        res
    }

    fn fresh_type_var(&mut self) -> TypeVarId {
        let id = TypeVarId(self.type_vars);
        self.type_vars += 1;
        id
    }

    fn with_scope<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let parent = std::mem::take(&mut self.scope);
        self.scope.parent = Some(Box::new(parent));
        let res = f(self);
        self.scope = *self.scope.parent.take().unwrap_or_default();
        res
    }

    #[inline(always)]
    fn new_type_param(&self, index: u32) -> TypeParamId {
        TypeParamId {
            depth: self.binder_depth,
            index,
        }
    }

    fn with_type_params<T>(&mut self, params: &[Symbol], f: impl FnOnce(&mut Self) -> T) -> T {
        self.with_scope(|this| {
            params.iter().copied().enumerate().for_each(|(i, symb)| {
                this.scope
                    .bind_type_param(symb, this.new_type_param(i as u32))
            });
            this.binder_depth += 1;
            let res = f(this);
            this.binder_depth -= 1;
            res
        })
    }

    pub fn resolve_structure(&mut self, structure: &Structure) -> Res<Vec<Item>> {
        structure
            .iter()
            .map(|item| self.resolve_structure_item(item))
            .collect()
    }

    fn resolve_structure_item(&mut self, item: &StructureItem) -> Res<Item> {
        match &item.desc {
            StructureItemDesc::Eval(expr) => self.resolve_eval(expr),
            StructureItemDesc::Value(rec_flag, value_bindings) => {
                self.resolve_value(rec_flag.is_rec(), value_bindings, item.span)
            }
            StructureItemDesc::Type(type_declarations) => {
                self.resolve_type_item(type_declarations, item.span)
            }
        }
    }

    fn resolve_type_path(&mut self, ident: &LongIdent, span: Span) -> Res<TypeId> {
        match ident {
            LongIdent::Ident(sym) => self.scope.resolve_type(*sym, span),
            LongIdent::Dot(..) => todo!("module paths"),
        }
    }

    fn resolve_eval(&mut self, _expr: &Expression) -> Res<Item> {
        todo!()
    }

    fn resolve_pattern(&mut self, pat: &parse_tree::pattern::Pattern) -> Res<Pattern> {
        let node = match &pat.desc {
            PatternDesc::Any => PatternNode::Wildcard,
            PatternDesc::Var(symbol) => {
                let var_id = self.vars.alloc(VarMarker);
                self.scope.bind_value(*symbol, ValueRef::Local(var_id));
                PatternNode::Var(var_id)
            }
            PatternDesc::Constant(_c) => todo!(),
            PatternDesc::Interval { .. } => todo!(),
            PatternDesc::Tuple(pats) => PatternNode::Tuple(
                pats.iter()
                    .map(|p| self.resolve_pattern(p))
                    .collect::<Res<_>>()?,
            ),
            PatternDesc::Construct(_cons, _arg) => todo!(),
            PatternDesc::Record(_record_fields) => todo!(),
            PatternDesc::Constraint(p, ty) => {
                let te = self.resolve_type_expr(ty)?;
                let p = self.resolve_pattern(p)?;
                PatternNode::Constraint(Box::new(p), te)
            }
            PatternDesc::Unit => PatternNode::Tuple(vec![]),
            PatternDesc::Paren(inner) => return self.resolve_pattern(inner),
            PatternDesc::BinaryOp(_op, _lhs, _rhs) => todo!(),
            PatternDesc::List(_locateds) => todo!(),
        };
        Ok(Pattern::new(node, pat.span))
    }

    // fn resolve_pattern_with_scope(
    //     &mut self,
    //     pat: &parse_tree::pattern::Pattern,
    // ) -> Res<(Scope, Pattern)> {
    //     let this_scope = mem::take(&mut self.scope);
    //     self.scope.parent = Some(Box::new(this_scope));
    //     let res = self.resolve_pattern(pat)?;
    //     let mut to_take = mem::take(&mut self.scope);
    //     let parent = to_take.parent.take().unwrap();
    //     self.scope = *parent;
    //     Ok((to_take, res))
    // }

    fn declare_value_binding(
        &mut self,
        binding: &parse_tree::expression::ValueBinding,
    ) -> Res<Pattern> {
        self.resolve_pattern(&binding.pat)
    }

    fn resolve_expression(&mut self, expr: &Expression) -> Res<Expr> {
        let span = expr.span;
        let node = match &expr.desc {
            ExpressionDesc::Ident(LongIdent::Ident(name)) => {
                let v_ref = self.scope.resolve_value(*name, span)?;
                ExprNode::Var(v_ref)
            }
            ExpressionDesc::Match { expr, with } => {
                let scrutinee = self.resolve_expression(expr)?;
                let arms = with
                    .iter()
                    .map(|arm| {
                        self.with_scope(|this| {
                            let pat = this.resolve_pattern(&arm.lhs)?;
                            let guard = arm
                                .guard
                                .as_ref()
                                .map(|g| this.resolve_expression(g))
                                .transpose()?
                                .map(Box::new);
                            let arm = this.resolve_expression(&arm.expr)?;
                            Ok(MatchCase {
                                pattern: pat,
                                guard,
                                body: arm,
                            })
                        })
                    })
                    .collect::<Res<_>>()?;

                ExprNode::Match {
                    scrutinee: Box::new(scrutinee),
                    cases: arms,
                }
            }
            ExpressionDesc::BinaryOp(op, lhs, rhs) => {
                let left = self.resolve_expression(lhs)?;
                let right = self.resolve_expression(rhs)?;
                ExprNode::BinaryOp {
                    op: *op,
                    left: Box::new(left),
                    right: Box::new(right),
                }
            }
            ExpressionDesc::Constant(x) => ExprNode::Const(*x),
            ExpressionDesc::Application(f, a) => {
                let func = self.resolve_expression(f)?;
                let arg = self.resolve_expression(a)?;
                ExprNode::Apply {
                    func: Box::new(func),
                    args: vec![arg],
                }
            }
            ExpressionDesc::Unit => ExprNode::Tuple(vec![]),
            ExpressionDesc::Product(_, _) => {
                let flattened = flatten_product(expr);
                assert!(!flattened.is_empty());
                if flattened.len() == 1 {
                    return self.resolve_expression(&flattened[0]);
                } else {
                    ExprNode::Tuple(
                        flattened
                            .iter()
                            .map(|x| self.resolve_expression(x))
                            .collect::<Res<_>>()?,
                    )
                }
            }
            x => todo!("{x:?}"),
        };
        Ok(Expr::new(node, span))
    }

    fn resolve_value_binding(
        &mut self,
        pat: Pattern,
        binding: &parse_tree::expression::ValueBinding,
    ) -> Res<ValueBinding> {
        self.with_scope(|this| {
            let mut params = vec![];

            for arg in &binding.args {
                params.push(this.resolve_pattern(arg)?);
            }

            let ty = binding
                .constraint
                .as_ref()
                .map(|ty| this.resolve_type_expr(&ty.typ))
                .transpose()?;

            let body = this.resolve_expression(&binding.expr)?;

            let (id, name, body) = match &pat.node {
                PatternNode::Var(id) => (*id, this.scope.lookup_var(*id).unwrap(), body),
                _ => {
                    assert!(params.is_empty());
                    let new_symbol = SESSION.lock().unwrap().fresh_symbol();
                    let id = this.vars.alloc(VarMarker);
                    this.scope.bind_value(new_symbol, ValueRef::Local(id));
                    let span = body.span;
                    let scrutinee = Expr::new(ExprNode::Var(ValueRef::Local(id)), span);
                    let new_body = Expr::new(
                        ExprNode::Match {
                            scrutinee: Box::new(scrutinee),
                            cases: vec![MatchCase {
                                pattern: pat,
                                guard: None,
                                body,
                            }],
                        },
                        span,
                    );
                    (id, new_symbol, new_body)
                }
            };

            let res = ValueBinding {
                id,
                name,
                params,
                ty,
                body,
            };

            Ok(res)
        })
    }

    fn resolve_value(
        &mut self,
        rec: bool,
        bindings: &[parse_tree::expression::ValueBinding],
        span: Span,
    ) -> Res<Item> {
        let bindings = if rec {
            let mut v = vec![];
            for binding in bindings {
                v.push(self.declare_value_binding(binding)?);
            }
            bindings
                .iter()
                .zip(v)
                .map(|(b, pat)| self.resolve_value_binding(pat, b))
                .collect::<Res<_>>()?
        } else {
            let mut res = vec![];
            for binding in bindings {
                let pat = self.declare_value_binding(binding)?;
                res.push(self.resolve_value_binding(pat, binding)?);
            }
            res
        };
        let node = ItemNode::Value {
            recursive: rec,
            bindings,
        };
        Ok(Item::new(node, span))
    }

    fn resolve_type_item(&mut self, tys: &[TypeDeclaration], span: Span) -> Res<Item> {
        let ids = self.declare_type_group(tys)?;
        let decls = tys
            .iter()
            .zip(ids)
            .map(|(ty, id)| self.resolve_type_decl(id, ty))
            .collect::<Res<_>>()?;
        Ok(Item {
            node: ItemNode::Type { decls },
            span,
        })
    }

    fn declare_type_group(&mut self, tys: &[TypeDeclaration]) -> Res<Vec<TypeId>> {
        let mut ids = Vec::new();
        for ty in tys {
            let id = self.types.alloc(TypeDeclInfo {
                name: ty.name.desc,
                arity: ty.params.len(),
            });
            ids.push(id);
            self.scope.bind_type(ty.name.desc, id);
            match &ty.kind {
                TypeKind::Variant(constructors) => {
                    for (i, cons) in constructors.iter().enumerate() {
                        let v_ref = ValueRef::Constructor {
                            type_id: id,
                            index: i as u32,
                        };
                        self.scope.bind_value(cons.name.desc, v_ref);
                    }
                }
                _ => todo!(),
            }
        }
        Ok(ids)
    }

    fn resolve_constructor(&mut self, c: &ConstructorDeclaration) -> Res<Constructor> {
        Ok(Constructor {
            name: c.name.desc,
            arg: c
                .args
                .as_ref()
                .map(|arg| match arg {
                    ConstructorArguments::TypeExpr(type_expr) => {
                        let resolved = self.resolve_type_expr(type_expr)?;
                        let tys = if resolved.as_ref().node.is_tuple() {
                            match resolved.node {
                                TypeNode::Tuple(tys) => tys,
                                _ => unreachable!(),
                            }
                        } else {
                            vec![resolved]
                        };
                        Ok(ConstructorArg::Tuple(tys))
                    }
                    _ => todo!(),
                })
                .transpose()?,
        })
    }

    fn resolve_type_decl(&mut self, id: TypeId, ty: &TypeDeclaration) -> Res<TypeDecl> {
        self.with_type_params(&ty.params, |this| {
            Ok(TypeDecl {
                id,
                name: ty.name.desc,
                params: ty.params.clone(),
                kind: match &ty.kind {
                    TypeKind::Variant(conss) => TypeDeclKind::Variant(
                        conss
                            .iter()
                            .map(|c| this.resolve_constructor(c))
                            .collect::<Res<_>>()?,
                    ),
                    TypeKind::Alias(expr) => {
                        let resolved = this.resolve_type_expr(expr)?;
                        TypeDeclKind::Alias(resolved)
                    }
                    _ => todo!(),
                },
            })
        })
    }

    fn resolve_type_expr(&mut self, type_expr: &TypeExpr) -> Res<Type> {
        let span = type_expr.span;
        let node = match &type_expr.desc {
            TypeExprDesc::Any => TypeNode::Infer,
            TypeExprDesc::Var(name) => {
                let id = self
                    .scope
                    .resolve_type_var(*name)
                    .unwrap_or_else(|| self.fresh_type_var());
                TypeNode::Var(id)
            }
            TypeExprDesc::Arrow(_, ty1, ty2) => {
                let r1 = self.resolve_type_expr(ty1)?;
                let r2 = self.resolve_type_expr(ty2)?;
                TypeNode::Arrow {
                    param: Box::new(r1),
                    result: Box::new(r2),
                }
            }
            TypeExprDesc::Tuple(tys) => TypeNode::Tuple(
                tys.iter()
                    .map(|ty| self.resolve_type_expr(ty))
                    .collect::<Res<_>>()?,
            ),
            TypeExprDesc::Constr(args, path) => {
                let r_args: Vec<_> = args
                    .iter()
                    .map(|ty| self.resolve_type_expr(ty))
                    .collect::<Res<_>>()?;
                let ty = self.resolve_type_path(path, span)?;
                TypeNode::Constr {
                    id: ty,
                    args: r_args,
                }
            }
            TypeExprDesc::Alias(_, _) => {
                todo!()
            }
        };
        Ok(Type::new(node, span))
    }
}

impl Default for Resolver {
    fn default() -> Self {
        Self::new()
    }
}
