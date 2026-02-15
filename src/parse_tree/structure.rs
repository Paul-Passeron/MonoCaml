use std::fmt;

use crate::{
    parse_tree::{
        Located,
        expression::{Expression, RecFlag, ValueBinding},
        type_declaration::TypeDeclaration,
    },
    resolve_symbol,
};

pub type Structure = Vec<StructureItem>;

pub type StructureItem = Located<StructureItemDesc>;

#[derive(Debug)]
pub enum StructureItemDesc {
    Eval(Expression),
    Value(RecFlag, Vec<ValueBinding>),
    Type(Vec<TypeDeclaration>),
}

const INDENT: &str = "    ";

pub struct StructureItemDescDisplay<'a> {
    pub desc: &'a StructureItemDesc,
    pub indent: usize,
}

impl StructureItemDesc {
    pub fn display<'a>(&'a self, indent: usize) -> StructureItemDescDisplay<'a> {
        StructureItemDescDisplay { desc: self, indent }
    }
}

impl fmt::Display for StructureItemDescDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.desc {
            StructureItemDesc::Eval(expr) => {
                write!(f, "{}", expr.desc.display(self.indent))
            }
            StructureItemDesc::Value(rec, bindings) => {
                write!(f, "{}", INDENT.repeat(self.indent))?;
                let mut iterator = bindings.iter();
                let first = iterator.next().expect("Malformed value structure item");
                write!(
                    f,
                    "let{} {}",
                    match rec {
                        RecFlag::Recursive => " rec",
                        RecFlag::NonRecursive => "",
                    },
                    first.pat.desc.display(0)
                )?;

                // Print arguments
                for arg in &first.args {
                    write!(f, " {}", arg.desc.display(0))?;
                }

                // Print constraint if present
                if let Some(constraint) = &first.constraint {
                    write!(f, " : ")?;
                    if !constraint.locally_abstract_types.is_empty() {
                        write!(f, "type ")?;
                        for (i, ty) in constraint.locally_abstract_types.iter().enumerate() {
                            if i > 0 {
                                write!(f, " ")?;
                            }
                            write!(f, "'{}", resolve_symbol(*ty))?;
                        }
                        write!(f, ". ")?;
                    }
                    write!(f, "{}", constraint.typ.desc.display(0))?;
                }

                writeln!(f, " =")?;
                write!(f, "{}", first.expr.desc.display(self.indent + 1))?;

                for binding in iterator {
                    write!(
                        f,
                        "\n{}and {}",
                        INDENT.repeat(self.indent),
                        binding.pat.desc.display(0)
                    )?;
                    for arg in &binding.args {
                        write!(f, " {}", arg.desc.display(0))?;
                    }
                    if let Some(constraint) = &binding.constraint {
                        write!(f, " : ")?;
                        if !constraint.locally_abstract_types.is_empty() {
                            write!(f, "type ")?;
                            for (i, ty) in constraint.locally_abstract_types.iter().enumerate() {
                                if i > 0 {
                                    write!(f, " ")?;
                                }
                                write!(f, "'{}", resolve_symbol(*ty))?;
                            }
                            write!(f, ". ")?;
                        }
                        write!(f, "{}", constraint.typ.desc.display(0))?;
                    }
                    writeln!(f, " =")?;
                    write!(f, "{}", binding.expr.desc.display(self.indent + 1))?;
                }
                Ok(())
            }
            StructureItemDesc::Type(type_decls) => {
                write!(f, "{}type ", INDENT.repeat(self.indent))?;
                for (i, type_decl) in type_decls.iter().enumerate() {
                    if i > 0 {
                        write!(f, "\n{}and ", INDENT.repeat(self.indent))?;
                    }
                    write!(f, "{}", type_decl.display(self.indent))?;
                }
                Ok(())
            }
        }
    }
}
