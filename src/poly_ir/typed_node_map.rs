use crate::{
    inference::InferenceCtx,
    poly_ir::{
        expr::{Expr, ExprNode, MatchCase, ValueBinding},
        item::{Item, ItemNode},
        pattern::{Pattern, PatternNode},
    },
};

pub trait Displayable {
    fn display(&self, ctx: &InferenceCtx) -> String;
}

impl<T> Pattern<T> {
    pub fn map<Ctx, R>(&self, ctx: &Ctx, f: &mut impl FnMut(&T, &Ctx) -> R) -> Pattern<R> {
        let span = self.span;
        let node = match &self.node {
            PatternNode::Wildcard => PatternNode::Wildcard,
            PatternNode::Var(id) => PatternNode::Var(*id),
            PatternNode::Tuple(pats) => {
                PatternNode::Tuple(pats.iter().map(|p| p.map(ctx, f)).collect())
            }
            PatternNode::Cons { ty, idx, arg } => PatternNode::Cons {
                ty: *ty,
                idx: *idx,
                arg: arg.as_ref().map(|p| Box::new(p.map(ctx, f))),
            },
        };
        let t = f(&self.ty, ctx);
        Pattern::new(node, span, t)
    }

    pub fn map_mut<Ctx, R>(
        &self,
        ctx: &mut Ctx,
        f: &mut impl FnMut(&T, &mut Ctx) -> R,
    ) -> Pattern<R> {
        let span = self.span;
        let node = match &self.node {
            PatternNode::Wildcard => PatternNode::Wildcard,
            PatternNode::Var(id) => PatternNode::Var(*id),
            PatternNode::Tuple(pats) => {
                PatternNode::Tuple(pats.iter().map(|p| p.map_mut(ctx, f)).collect())
            }
            PatternNode::Cons { ty, idx, arg } => PatternNode::Cons {
                ty: *ty,
                idx: *idx,
                arg: arg.as_ref().map(|p| Box::new(p.map_mut(ctx, f))),
            },
        };
        let t = f(&self.ty, ctx);
        Pattern::new(node, span, t)
    }
}

impl<T> ValueBinding<T> {
    pub fn map<Ctx, R>(&self, ctx: &Ctx, f: &mut impl FnMut(&T, &Ctx) -> R) -> ValueBinding<R> {
        let pat = self.pat.map(ctx, f);
        let params = self.params.iter().map(|pat| pat.map(ctx, f)).collect();
        let ty = f(&self.ty, ctx);
        let body = self.body.map(ctx, f);
        ValueBinding {
            id: self.id,
            pat,
            params,
            ty,
            body,
        }
    }

    pub fn map_mut<Ctx, R>(
        &self,
        ctx: &mut Ctx,
        f: &mut impl FnMut(&T, &mut Ctx) -> R,
    ) -> ValueBinding<R> {
        let pat = self.pat.map_mut(ctx, f);
        let params = self.params.iter().map(|pat| pat.map_mut(ctx, f)).collect();
        let ty = f(&self.ty, ctx);
        let body = self.body.map_mut(ctx, f);
        ValueBinding {
            id: self.id,
            pat,
            params,
            ty,
            body,
        }
    }
}

impl<T> MatchCase<T> {
    pub fn map<Ctx, R>(&self, ctx: &Ctx, f: &mut impl FnMut(&T, &Ctx) -> R) -> MatchCase<R> {
        let pattern = self.pattern.map(ctx, f);
        let guard = self.guard.as_ref().map(|g| g.map(ctx, f));
        let body = self.body.map(ctx, f);
        MatchCase {
            pattern,
            guard: guard.map(Box::new),
            body,
        }
    }

    pub fn map_mut<Ctx, R>(
        &self,
        ctx: &mut Ctx,
        f: &mut impl FnMut(&T, &mut Ctx) -> R,
    ) -> MatchCase<R> {
        let pattern = self.pattern.map_mut(ctx, f);
        let guard = self.guard.as_ref().map(|g| g.map_mut(ctx, f));
        let body = self.body.map_mut(ctx, f);
        MatchCase {
            pattern,
            guard: guard.map(Box::new),
            body,
        }
    }
}

impl<T> Expr<T> {
    pub fn map<Ctx, R>(&self, ctx: &Ctx, f: &mut impl FnMut(&T, &Ctx) -> R) -> Expr<R> {
        let span = self.span;
        let t = f(&self.ty, ctx);
        let node = match self.as_ref().node {
            ExprNode::Var(value_ref) => ExprNode::Var(*value_ref),
            ExprNode::Const(constant) => ExprNode::Const(*constant),
            ExprNode::Let {
                recursive,
                bindings,
                body,
            } => {
                let bindings = bindings.iter().map(|b| b.map(ctx, f)).collect();
                let body = body.map(ctx, f);
                ExprNode::Let {
                    recursive: *recursive,
                    bindings,
                    body: Box::new(body),
                }
            }
            // ExprNode::Function { cases } => ExprNode::Function {
            //     cases: cases.iter().map(|x| x.map(ctx, f)).collect(),
            // },
            ExprNode::Apply { func, arg } => ExprNode::Apply {
                func: Box::new(func.map(ctx, f)),
                arg: Box::new(arg.map(ctx, f)),
            },
            ExprNode::Match { scrutinee, cases } => ExprNode::Match {
                scrutinee: Box::new(scrutinee.map(ctx, f)),
                cases: cases.iter().map(|x| x.map(ctx, f)).collect(),
            },
            ExprNode::Tuple(pats) => {
                ExprNode::Tuple(pats.iter().map(|pat| pat.map(ctx, f)).collect())
            }
            ExprNode::Construct { ty, idx, arg } => ExprNode::Construct {
                ty: *ty,
                idx: *idx,
                arg: arg.as_ref().map(|x| Box::new(x.map(ctx, f))),
            },
            ExprNode::Sequence { first, second } => ExprNode::Sequence {
                first: Box::new(first.map(ctx, f)),
                second: Box::new(second.map(ctx, f)),
            },
            ExprNode::Constraint { expr, ty } => ExprNode::Constraint {
                expr: Box::new(expr.map(ctx, f)),
                ty: f(ty, ctx),
            },
            ExprNode::BinaryOp { op, left, right } => ExprNode::BinaryOp {
                op: *op,
                left: Box::new(left.map(ctx, f)),
                right: Box::new(right.map(ctx, f)),
            },
            ExprNode::UnaryOp { op, expr } => ExprNode::UnaryOp {
                op: *op,
                expr: Box::new(expr.map(ctx, f)),
            },
            ExprNode::Unit => ExprNode::Unit,
            ExprNode::IfThenElse {
                cond,
                then_expr,
                else_expr,
            } => ExprNode::IfThenElse {
                cond: Box::new(cond.map(ctx, f)),
                then_expr: Box::new(then_expr.map(ctx, f)),
                else_expr: Box::new(else_expr.map(ctx, f)),
            },
            // ExprNode::Fun { arg, body } => ExprNode::Fun {
            //     arg: arg.map(ctx, f),
            //     body: Box::new(body.map(ctx, f)),
            // },
        };
        Expr::new(node, span, t)
    }

    pub fn map_mut<Ctx, R>(&self, ctx: &mut Ctx, f: &mut impl FnMut(&T, &mut Ctx) -> R) -> Expr<R> {
        let span = self.span;
        let t = f(&self.ty, ctx);
        let node = match self.as_ref().node {
            ExprNode::Var(value_ref) => ExprNode::Var(*value_ref),
            ExprNode::Const(constant) => ExprNode::Const(*constant),
            ExprNode::Let {
                recursive,
                bindings,
                body,
            } => {
                let bindings = bindings.iter().map(|b| b.map_mut(ctx, f)).collect();
                let body = body.map_mut(ctx, f);
                ExprNode::Let {
                    recursive: *recursive,
                    bindings,
                    body: Box::new(body),
                }
            }
            // ExprNode::Function { cases } => ExprNode::Function {
            //     cases: cases.iter().map(|x| x.map_mut(ctx, f)).collect(),
            // },
            ExprNode::Apply { func, arg } => ExprNode::Apply {
                func: Box::new(func.map_mut(ctx, f)),
                arg: Box::new(arg.map_mut(ctx, f)),
            },
            ExprNode::Match { scrutinee, cases } => ExprNode::Match {
                scrutinee: Box::new(scrutinee.map_mut(ctx, f)),
                cases: cases.iter().map(|x| x.map_mut(ctx, f)).collect(),
            },
            ExprNode::Tuple(pats) => {
                ExprNode::Tuple(pats.iter().map(|pat| pat.map_mut(ctx, f)).collect())
            }
            ExprNode::Construct { arg, ty, idx } => ExprNode::Construct {
                ty: *ty,
                idx: *idx,
                arg: arg.as_ref().map(|x| Box::new(x.map_mut(ctx, f))),
            },
            ExprNode::Sequence { first, second } => ExprNode::Sequence {
                first: Box::new(first.map_mut(ctx, f)),
                second: Box::new(second.map_mut(ctx, f)),
            },
            ExprNode::Constraint { expr, ty } => ExprNode::Constraint {
                expr: Box::new(expr.map_mut(ctx, f)),
                ty: f(ty, ctx),
            },
            ExprNode::BinaryOp { op, left, right } => ExprNode::BinaryOp {
                op: *op,
                left: Box::new(left.map_mut(ctx, f)),
                right: Box::new(right.map_mut(ctx, f)),
            },
            ExprNode::UnaryOp { op, expr } => ExprNode::UnaryOp {
                op: *op,
                expr: Box::new(expr.map_mut(ctx, f)),
            },
            ExprNode::Unit => ExprNode::Unit,
            ExprNode::IfThenElse {
                cond,
                then_expr,
                else_expr,
            } => ExprNode::IfThenElse {
                cond: Box::new(cond.map_mut(ctx, f)),
                then_expr: Box::new(then_expr.map_mut(ctx, f)),
                else_expr: Box::new(else_expr.map_mut(ctx, f)),
            },
            // ExprNode::Fun { arg, body } => ExprNode::Fun {
            //     arg: arg.map_mut(ctx, f),
            //     body: Box::new(body.map_mut(ctx, f)),
            // },
        };
        Expr::new(node, span, t)
    }
}

impl<T> Item<T> {
    pub fn map<Ctx, R>(&self, ctx: &Ctx, f: &mut impl FnMut(&T, &Ctx) -> R) -> Item<R> {
        let span = self.span;
        let node = match self.as_ref().node {
            ItemNode::Value {
                recursive,
                bindings,
            } => ItemNode::Value {
                recursive: *recursive,
                bindings: bindings.iter().map(|x| x.map(ctx, f)).collect(),
            },
            ItemNode::Type { decls } => ItemNode::Type {
                decls: decls.clone(),
            },
        };
        Item::new(node, span, ())
    }

    pub fn map_mut<Ctx, R>(&self, ctx: &mut Ctx, f: &mut impl FnMut(&T, &mut Ctx) -> R) -> Item<R> {
        let span = self.span;
        let node = match self.as_ref().node {
            ItemNode::Value {
                recursive,
                bindings,
            } => ItemNode::Value {
                recursive: *recursive,
                bindings: bindings.iter().map(|x| x.map_mut(ctx, f)).collect(),
            },
            ItemNode::Type { decls } => ItemNode::Type {
                decls: decls.clone(),
            },
        };
        Item::new(node, span, ())
    }
}
