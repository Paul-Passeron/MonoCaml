use std::collections::HashMap;

use crate::{
    lexer::interner::Symbol,
    poly_ir::{TypeId, TypeParamId, ValueRef, type_expr::TypeVarId},
    resolution::error::{Res, ResolutionError},
    source_manager::loc::Span,
};

pub struct Scope {
    values: HashMap<Symbol, ValueRef>,
    types: HashMap<Symbol, TypeId>,
    type_params: HashMap<Symbol, TypeParamId>,
    type_vars: HashMap<Symbol, TypeVarId>,
    pub parent: Option<Box<Self>>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            types: HashMap::new(),
            type_params: HashMap::new(),
            type_vars: HashMap::new(),
            parent: None,
        }
    }

    pub fn with_parent(parent: Self) -> Self {
        Self {
            values: HashMap::new(),
            types: HashMap::new(),
            type_params: HashMap::new(),
            type_vars: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }

    pub fn resolve_value(&self, name: Symbol, span: Span) -> Res<ValueRef> {
        self.values
            .get(&name)
            .copied()
            .map(Ok)
            .or_else(|| {
                self.parent
                    .as_ref()
                    .map(|parent| parent.resolve_value(name, span))
            })
            .transpose()?
            .ok_or_else(|| ResolutionError::UnboundValue {
                name: name,
                span: span,
            })
    }

    pub fn resolve_type(&self, name: Symbol, span: Span) -> Res<TypeId> {
        self.types
            .get(&name)
            .copied()
            .map(Ok)
            .or_else(|| {
                self.parent
                    .as_ref()
                    .map(|parent| parent.resolve_type(name, span))
            })
            .transpose()?
            .ok_or_else(|| ResolutionError::UnboundType {
                name: name,
                span: span,
            })
    }

    pub fn resolve_type_param(&self, name: Symbol, span: Span) -> Res<TypeParamId> {
        self.type_params
            .get(&name)
            .copied()
            .map(Ok)
            .or_else(|| {
                self.parent
                    .as_ref()
                    .map(|parent| parent.resolve_type_param(name, span))
            })
            .transpose()?
            .ok_or_else(|| ResolutionError::UnboundTypeParam {
                name: name,
                span: span,
            })
    }

    pub fn resolve_type_var(&self, name: Symbol, span: Span) -> Option<TypeVarId> {
        self.type_vars.get(&name).copied().or_else(|| {
            self.parent
                .as_ref()
                .map(|parent| parent.resolve_type_var(name, span))
                .flatten()
        })
    }

    pub fn bind_value(&mut self, name: Symbol, binding: ValueRef) {
        self.values.insert(name, binding);
    }

    pub fn bind_type(&mut self, name: Symbol, binding: TypeId) {
        self.types.insert(name, binding);
    }

    pub fn bind_type_param(&mut self, name: Symbol, binding: TypeParamId) {
        self.type_params.insert(name, binding);
    }

    pub fn set_parent(&mut self, parent: Self) {
        self.parent = Some(Box::new(parent))
    }
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}
