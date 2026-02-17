use std::collections::HashMap;

use crate::{
    lexer::interner::Symbol,
    poly_ir::{TypeId, TypeParamId, ValueRef, VarId, type_expr::TypeVarId},
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
            .ok_or(ResolutionError::UnboundValue { name, span })
    }

    pub fn resolve_type(&self, name: Symbol, span: Span) -> Res<TypeId> {
        self.types
            .get(&name)
            .copied()
            .map(Ok)
            .or(self
                .parent
                .as_ref()
                .map(|parent| parent.resolve_type(name, span)))
            .transpose()?
            .ok_or(ResolutionError::UnboundType { name, span })
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
            .ok_or(ResolutionError::UnboundTypeParam { name, span })
    }

    pub fn resolve_type_var(&self, name: Symbol) -> Option<TypeVarId> {
        self.type_vars.get(&name).copied().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.resolve_type_var(name))
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

    pub fn lookup_var(&self, id: VarId) -> Option<Symbol> {
        self.values
            .iter()
            .find_map(|(k, v)| match v {
                ValueRef::Local(lid) if id == *lid => Some(Some(*k)),
                _ => None,
            })
            .unwrap_or_else(|| {
                self.parent
                    .as_ref()
                    .and_then(|parent| parent.lookup_var(id))
            })
    }
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}
