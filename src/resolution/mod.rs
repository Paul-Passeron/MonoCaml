use crate::{
    lexer::interner::Symbol,
    parse_tree::{
        self, LongIdent,
        expression::Expression,
        structure::{Structure, StructureItem, StructureItemDesc},
        type_declaration::{
            ConstructorArguments, ConstructorDeclaration, TypeDeclaration, TypeKind,
        },
        type_expr::{TypeExpr, TypeExprDesc},
    },
    poly_ir::{
        TypeId, TypeParamId, ValueRef, VarMarker,
        expr::ValueBinding,
        id::Arena,
        item::{
            Constructor, ConstructorArg, Item, ItemInfo, ItemNode, TypeDecl, TypeDeclInfo,
            TypeDeclKind,
        },
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
    pub items: Arena<ItemInfo>,
    scope: Scope,
    binder_depth: u32,
    type_vars: u32,
}

impl Resolver {
    pub fn new() -> Self {
        let mut res = Self {
            types: Arena::new(),
            vars: Arena::new(),
            items: Arena::new(),
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

    fn resolve_eval(&mut self, expr: &Expression) -> Res<Item> {
        todo!()
    }

    fn declare_value_binding(&mut self, binding: &parse_tree::expression::ValueBinding) -> Res<()> {
        todo!()
    }

    fn resolve_value_binding(
        &mut self,
        binding: &parse_tree::expression::ValueBinding,
    ) -> Res<ValueBinding> {
        todo!()
    }

    fn resolve_value(
        &mut self,
        rec: bool,
        bindings: &[parse_tree::expression::ValueBinding],
        span: Span,
    ) -> Res<Item> {
        let bindings = if rec {
            for binding in bindings {
                self.declare_value_binding(binding)?;
            }
            bindings
                .iter()
                .map(|b| self.resolve_value_binding(b))
                .collect::<Res<_>>()?
        } else {
            todo!()
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
            TypeExprDesc::Alias(ty_expr, name) => {
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
