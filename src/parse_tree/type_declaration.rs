use std::fmt;

use crate::{
    lexer::interner::Symbol,
    parse_tree::{Located, type_expr::TypeExpr},
    session::Session,
    source_manager::loc::Loc,
};

#[derive(Debug)]
pub struct TypeDeclaration {
    pub name: Located<Symbol>,
    pub params: Vec<Symbol>,
    pub kind: TypeKind,
}

#[derive(Debug)]
pub enum TypeKind {
    Alias(TypeExpr),
    Variant(Vec<ConstructorDeclaration>),
    Record(Vec<LabelDeclaration>),
}

#[derive(Debug)]
pub struct ConstructorDeclaration {
    pub name: Located<Symbol>,
    pub args: Option<ConstructorArguments>,
    pub loc: Loc,
}

#[derive(Debug)]
pub enum ConstructorArguments {
    TypeExpr(TypeExpr),
    Record(Vec<LabelDeclaration>),
}

#[derive(Debug)]
pub enum MutableFlag {
    Mut,
    NonMut,
}

#[derive(Debug)]
pub struct LabelDeclaration {
    pub name: Located<Symbol>,
    pub mutable: MutableFlag,
    pub typ: TypeExpr,
    pub loc: Loc,
}

const INDENT: &'static str = "    ";

pub struct TypeDeclarationDisplay<'a, 'b> {
    pub decl: &'a TypeDeclaration,
    pub session: &'b Session,
    pub indent: usize,
}

impl TypeDeclaration {
    pub fn display<'a, 'b>(
        &'a self,
        session: &'b Session,
        indent: usize,
    ) -> TypeDeclarationDisplay<'a, 'b> {
        TypeDeclarationDisplay {
            decl: self,
            session,
            indent,
        }
    }
}

impl fmt::Display for TypeDeclarationDisplay<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.decl.params.is_empty() {
            if self.decl.params.len() == 1 {
                write!(
                    f,
                    "'{} ",
                    self.decl.params[0].display(&self.session.symbol_interner)
                )?;
            } else {
                write!(f, "(")?;
                for (i, param) in self.decl.params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "'{}", param.display(&self.session.symbol_interner))?;
                }
                write!(f, ") ")?;
            }
        }

        write!(
            f,
            "{}",
            self.decl.name.desc.display(&self.session.symbol_interner)
        )?;

        match &self.decl.kind {
            TypeKind::Alias(typ) => {
                write!(f, " = {}", typ.desc.display(self.session, 0))
            }
            TypeKind::Variant(constructors) => {
                write!(f, " =")?;
                for (i, cons) in constructors.iter().enumerate() {
                    if i == 0 {
                        write!(f, "\n{}", INDENT.repeat(self.indent + 1))?;
                    } else {
                        write!(
                            f,
                            "\n{}| ",
                            &INDENT.repeat(self.indent + 1).to_string()[2..]
                        )?;
                    }
                    write!(
                        f,
                        "{}",
                        cons.name.desc.display(&self.session.symbol_interner)
                    )?;

                    match &cons.args {
                        Some(ConstructorArguments::TypeExpr(typ)) => {
                            write!(f, " of {}", typ.desc.display(self.session, 0))?;
                        }
                        Some(ConstructorArguments::Record(labels)) => {
                            write!(f, " of {{ ")?;
                            for (i, label) in labels.iter().enumerate() {
                                if i > 0 {
                                    write!(f, "; ")?;
                                }
                                write!(
                                    f,
                                    "{}",
                                    label.name.desc.display(&self.session.symbol_interner)
                                )?;
                                write!(f, " : ")?;
                                write!(f, "{}", label.typ.desc.display(self.session, 0))?;
                            }
                            write!(f, " }}")?;
                        }
                        None => (),
                    }
                }
                Ok(())
            }
            TypeKind::Record(labels) => {
                write!(f, " = {{")?;
                for (i, label) in labels.iter().enumerate() {
                    if i > 0 {
                        write!(f, ";")?;
                    }
                    write!(f, "\n{}", INDENT.repeat(self.indent + 1))?;
                    match label.mutable {
                        MutableFlag::Mut => write!(f, "mutable ")?,
                        MutableFlag::NonMut => {}
                    }
                    write!(
                        f,
                        "{} : {}",
                        label.name.desc.display(&self.session.symbol_interner),
                        label.typ.desc.display(self.session, 0)
                    )?;
                }
                write!(f, "\n{}}}", INDENT.repeat(self.indent))
            }
        }
    }
}
