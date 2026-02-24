use crate::{
    SESSION,
    lexer::interner::Symbol,
    parse_tree::{
        self, LongIdent,
        expression::{Expression, ExpressionDesc},
        pattern::{self, PatternDesc},
        structure::{Structure, StructureItem, StructureItemDesc},
        type_declaration::{
            ConstructorArguments, ConstructorDeclaration, TypeDeclaration, TypeKind,
        },
        type_expr::{TypeExpr, TypeExprDesc},
    },
    poly_ir::{
        TypeId, TypeParamId, ValueRef,
        expr::{Expr, ExprNode, MatchCase, ValueBinding},
        id::Arena,
        item::{Constructor, ConstructorArg, Item, ItemNode, TypeDecl, TypeDeclInfo, TypeDeclKind},
        pattern::{Pattern, PatternNode},
        type_expr::{Type, TypeVarId},
    },
    resolution::{error::Res, scope::Scope},
    source_manager::loc::Span,
};

pub mod builtins;
pub mod error;
pub mod scope;

pub type ResType = Type;
pub type ResItem = Item<ResType>;
pub type ResExpr = Expr<ResType>;
pub type ResPattern = Pattern<ResType>;
pub type ResValueBinding = ValueBinding<ResType>;

#[derive(Debug)]
pub struct VarInfo {
    pub name: Symbol,
}

pub struct Resolver {
    pub types: Arena<TypeDeclInfo>,
    pub vars: Arena<VarInfo>,
    scope: Scope,
    binder_depth: u32,
    type_vars: u32,
}

fn flatten_product(expr: &Expression) -> Vec<&Expression> {
    fn aux<'a>(e: &'a Expression, v: &mut Vec<&'a Expression>) {
        match &e.desc {
            ExpressionDesc::Product(x1, x2) => {
                // v.push(x1);
                // aux(x2, v);
                aux(x1, v);
                v.push(x2);
            }
            _ => v.push(e),
        }
    }
    let mut v = vec![];
    aux(expr, &mut v);
    v
}

fn flatten_product_pattern(pat: &pattern::Pattern) -> Vec<&pattern::Pattern> {
    fn aux<'a>(p: &'a pattern::Pattern, v: &mut Vec<&'a pattern::Pattern>) {
        match &p.desc {
            PatternDesc::Product(x1, x2) => {
                aux(x1, v);
                v.push(x2)
            }
            _ => v.push(p),
        }
    }
    let mut v = vec![];
    aux(pat, &mut v);
    dbg!(v)
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

    pub fn resolve_structure(&mut self, structure: &Structure) -> Res<Vec<ResItem>> {
        structure
            .iter()
            .map(|item| self.resolve_structure_item(item))
            .collect()
    }

    fn resolve_structure_item(&mut self, item: &StructureItem) -> Res<ResItem> {
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

    fn resolve_eval(&mut self, expr: &Expression) -> Res<ResItem> {
        let resolved = self.resolve_expression(expr)?;
        let new_symbol = SESSION.lock().unwrap().fresh_symbol();
        let id = self.vars.alloc(VarInfo { name: new_symbol });
        self.scope.bind_value(new_symbol, ValueRef::Local(id));
        let span = resolved.span;
        Ok(ResItem::new(
            ItemNode::Value {
                recursive: false,
                bindings: vec![ResValueBinding {
                    pat: ResPattern::new(PatternNode::Wildcard, span, Type::default()),
                    params: vec![],
                    ty: resolved.ty.clone(),
                    body: resolved,
                }],
            },
            span,
            (),
        ))
    }

    fn resolve_pattern(&mut self, pat: &parse_tree::pattern::Pattern) -> Res<ResPattern> {
        let mut opt_ty = None;
        let node = match &pat.desc {
            PatternDesc::Any => PatternNode::Wildcard,
            PatternDesc::Var(symbol) => {
                let var_id = self.vars.alloc(VarInfo { name: *symbol });
                self.scope.bind_value(*symbol, ValueRef::Local(var_id));
                PatternNode::Var(var_id)
            }
            PatternDesc::Constant(_c) => todo!(),
            PatternDesc::Interval { .. } => todo!(),
            PatternDesc::Product(_, _) => PatternNode::Tuple(
                flatten_product_pattern(pat)
                    .iter()
                    .map(|x| self.resolve_pattern(x))
                    .collect::<Res<_>>()?,
            ),
            PatternDesc::Construct(_cons, _arg) => todo!(),
            PatternDesc::Record(_record_fields) => todo!(),
            PatternDesc::Constraint(p, ty) => {
                let te = self.resolve_type_expr(ty)?;
                let p = self.resolve_pattern(p)?;
                opt_ty = Some(te);
                p.node
            }
            PatternDesc::Unit => PatternNode::Tuple(vec![]),
            PatternDesc::Paren(inner) => return self.resolve_pattern(inner),
            PatternDesc::BinaryOp(_op, _lhs, _rhs) => todo!(),
            PatternDesc::List(_locateds) => todo!(),
        };
        Ok(Pattern::new(node, pat.span, opt_ty.unwrap_or_default()))
    }

    fn declare_value_binding(
        &mut self,
        binding: &parse_tree::expression::ValueBinding,
    ) -> Res<ResPattern> {
        self.resolve_pattern(&binding.pat)
    }

    fn resolve_expression(&mut self, expr: &Expression) -> Res<ResExpr> {
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
                    arg: Box::new(arg),
                }
            }
            ExpressionDesc::Unit => ExprNode::Tuple(vec![]),
            ExpressionDesc::Product(_, _) => ExprNode::Tuple(
                flatten_product(expr)
                    .iter()
                    .map(|x| self.resolve_expression(x))
                    .collect::<Res<_>>()?,
            ),
            ExpressionDesc::Paren(e) => return self.resolve_expression(e),
            ExpressionDesc::IfThenElse {
                cond,
                then_expr,
                else_expr,
            } => {
                let r_cond = self.resolve_expression(cond)?;
                let r_then_expr = self.resolve_expression(then_expr)?;
                let r_else_expr = else_expr
                    .as_ref()
                    .map(|expr| self.resolve_expression(expr))
                    .transpose()?
                    .unwrap_or_else(|| {
                        let l = r_then_expr.span.split().1;
                        let span = l.span(&l);
                        Expr::new(ExprNode::Unit, span, Default::default())
                    });
                ExprNode::IfThenElse {
                    cond: Box::new(r_cond),
                    then_expr: Box::new(r_then_expr),
                    else_expr: Box::new(r_else_expr),
                }
            }
            x => todo!("{x:?}"),
        };
        Ok(Expr::new(node, span, Type::default()))
    }

    fn resolve_value_binding(
        &mut self,
        pat: ResPattern,
        binding: &parse_tree::expression::ValueBinding,
    ) -> Res<ResValueBinding> {
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

            let res = ValueBinding {
                pat,
                params,
                ty: ty.unwrap_or_default(),
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
    ) -> Res<ResItem> {
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
                let (params, ty, body) = self.with_scope(|this| {
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

                    Ok((params, ty.unwrap_or_default(), body))
                })?;
                let pat = self.declare_value_binding(binding)?;
                let b = ValueBinding {
                    pat,
                    params,
                    ty,
                    body,
                };
                res.push(b);
            }
            res
        };
        let node = ItemNode::Value {
            recursive: rec,
            bindings,
        };
        Ok(Item::new(node, span, ()))
    }

    fn resolve_type_item(&mut self, tys: &[TypeDeclaration], span: Span) -> Res<ResItem> {
        let ids = self.declare_type_group(tys)?;
        let decls = tys
            .iter()
            .zip(ids)
            .map(|(ty, id)| self.resolve_type_decl(id, ty))
            .collect::<Res<_>>()?;
        Ok(Item::new(ItemNode::Type { decls }, span, ()))
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
                TypeKind::Alias(_) => (),
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
                        let tys = if resolved.is_tuple() {
                            match resolved {
                                Type::Tuple(tys) => tys,
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
        Ok(match &type_expr.desc {
            TypeExprDesc::Any => Type::Infer,
            TypeExprDesc::Var(name) => {
                let id = self
                    .scope
                    .resolve_type_var(*name)
                    .unwrap_or_else(|| self.fresh_type_var());
                Type::Var(id)
            }
            TypeExprDesc::Arrow(_, ty1, ty2) => {
                let r1 = self.resolve_type_expr(ty1)?;
                let r2 = self.resolve_type_expr(ty2)?;
                Type::Arrow {
                    param: Box::new(r1),
                    result: Box::new(r2),
                }
            }
            TypeExprDesc::Tuple(tys) => Type::Tuple(
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
                Type::Constr {
                    id: ty,
                    args: r_args,
                }
            }
            TypeExprDesc::Alias(_, _) => {
                todo!()
            }
        })
    }
}

impl Default for Resolver {
    fn default() -> Self {
        Self::new()
    }
}
