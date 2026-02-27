use std::collections::HashMap;

use crate::{
    cfg::{Const, FunNameUse, Value, builder::Builder},
    lower::mono_to_cfg::MonoToCfg,
    monomorph::ConcrTy,
    parse_tree::expression::Constant,
    poly_ir::{
        ValueRef, VarId,
        expr::{Expr, ExprNode},
    },
    resolve_strlit,
};

#[allow(unused)]
impl<'b> MonoToCfg<'b> {
    fn compile_constant(&self, constant: Constant, b: &mut Builder) -> Const {
        match constant {
            Constant::Int(x) => Const::Int(x as i32),
            Constant::Char(_) => todo!(),
            Constant::String(str_lit) => Const::String(resolve_strlit(str_lit)),
            Constant::Float(_) => todo!(),
            Constant::Bool(_) => todo!(),
        }
    }

    pub(super) fn compile_expr(
        &mut self,
        expr: &Expr<ConcrTy>,
        subst: &HashMap<VarId, Value>,
        b: &mut Builder,
    ) -> Value {
        match &expr.node {
            ExprNode::Var(ValueRef::Local(id)) => todo!(),
            ExprNode::Const(constant) => self.compile_constant(*constant, b).into(),
            ExprNode::Let {
                recursive,
                bindings,
                body,
            } => {
                let mut new_subst = subst.clone();
                self.compile_bindings(*recursive, bindings, &mut new_subst, b);
                self.compile_expr(body, &new_subst, b)
            }
            ExprNode::Apply { func, arg } => {
                if let Some((fun, args)) = self.get_saturated(expr) {
                    let args = args
                        .iter()
                        .map(|x| self.compile_expr(x, subst, b))
                        .collect();
                    b.native_call(&mut self.ctx, Value::constant(Const::FunPtr(fun)), args)
                        .into()
                } else {
                    let f = self.compile_expr(func, subst, b);
                    let a = self.compile_expr(arg, subst, b);
                    todo!()
                }
            }
            ExprNode::Match { scrutinee, cases } => todo!(),
            ExprNode::Tuple(typed_nodes) => todo!(),
            ExprNode::Construct { ty, idx, arg } => todo!(),
            ExprNode::Sequence { first, second } => todo!(),
            ExprNode::Constraint { expr, ty } => todo!(),
            ExprNode::BinaryOp { op, left, right } => todo!(),
            ExprNode::UnaryOp { op, expr } => todo!(),
            ExprNode::Unit => todo!(),
            ExprNode::IfThenElse {
                cond,
                then_expr,
                else_expr,
            } => todo!(),
            ExprNode::Var(ValueRef::Constructor { type_id, index }) => todo!(),
        }
    }

    fn get_saturated<'a>(
        &self,
        expr: &'a Expr<ConcrTy>,
    ) -> Option<(FunNameUse, Vec<&'a Expr<ConcrTy>>)> {
        fn aux<'a>(
            expr: &'a Expr<ConcrTy>,
            args: &mut Vec<&'a Expr<ConcrTy>>,
            ctx: &MonoToCfg,
        ) -> Option<FunNameUse> {
            match &expr.node {
                ExprNode::Var(ValueRef::Local(var)) => {
                    if let Some(fun) = ctx.builtins.get(var) {
                        if args.len() == ctx.ctx.sigs[fun].params().len() {
                            return Some(*fun);
                        }
                    } else {
                        println!("Did not found {} to be builtin.", ctx.var_arena[*var].name)
                    }
                    None
                }
                ExprNode::Apply { func, arg } => {
                    args.push(arg);
                    aux(&func, args, ctx)
                }
                _ => None,
            }
        }
        let mut v = vec![];
        aux(expr, &mut v, self).map(|fun| {
            v.reverse();
            (fun, v)
        })
    }
}
