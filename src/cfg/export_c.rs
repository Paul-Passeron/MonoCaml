use crate::cfg::{Cfg, Program, Ty};
use std::collections::{HashMap, HashSet};
use std::fmt::Write as FmtWrite;
use std::io::Write;

#[derive(Default)]
pub struct ExportC {
    pub funs: String,
    pub types: String,
    pub prelude: String,
    pub type_names: HashMap<Ty, String>,
}

impl ExportC {
    pub fn define_type(&mut self, ty: Ty) {
        if self.type_names.contains_key(&ty) {
            return;
        }
        let s = match &ty {
            Ty::Int => "int".to_string(),
            Ty::String => "const char *".to_string(),
            Ty::Void => "void".to_string(),
            Ty::Ptr(ty) => {
                self.define_type(*ty.clone());
                format!("{}*", self.type_names[ty])
            }
            Ty::Struct(items) => {
                items.iter().cloned().for_each(|x| self.define_type(x));
                let f = &mut self.types;
                writeln!(f, "typedef struct {{").unwrap();
                items
                    .iter()
                    .cloned()
                    .enumerate()
                    .for_each(|(i, x)| writeln!(f, "{} _{i};", &self.type_names[&x]).unwrap());
                let name = format!("ty_{}", self.type_names.len());
                writeln!(f, "}} {name};").unwrap();
                name
            }
            Ty::FunPtr(sig) => {
                self.define_type(sig.ret.as_ref().clone());
                sig.params.iter().cloned().for_each(|x| self.define_type(x));
                let name = format!("ty_{}", self.type_names.len());
                let f = &mut self.types;
                writeln!(
                    f,
                    "typedef {} (*{})({});",
                    self.type_names[sig.ret.as_ref()],
                    name,
                    sig.params
                        .iter()
                        .cloned()
                        .map(|x| self.type_names[&x].to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
                .unwrap();
                name
            }
        };
        self.type_names.insert(ty, s);
    }

    pub fn export(&mut self, prog: &Program) {
        let types = prog.get_all_types();
        for ty in types {
            self.define_type(ty);
        }

        println!("\nTYPES\n{}\n", self.types);
    }
}

impl Cfg {
    pub fn get_all_types(&self) -> HashSet<Ty> {
        HashSet::from_iter(self.locals.values().cloned())
    }
}

fn add_type(s: &mut HashSet<Ty>, ty: &Ty) {
    if s.contains(ty) {
        return;
    }
    s.insert(ty.clone());
    match ty {
        Ty::Ptr(ty) => add_type(s, ty),
        Ty::Struct(items) => items.iter().for_each(|x| add_type(s, x)),
        Ty::FunPtr(sig) => {
            add_type(s, &sig.ret);
            sig.params.iter().for_each(|x| add_type(s, x));
        }
        _ => (),
    }
}

impl Program {
    fn get_all_types(&self) -> HashSet<Ty> {
        let mut s = HashSet::new();
        for f in &self.funcs {
            f.params.iter().for_each(|(_, x)| add_type(&mut s, x));
            add_type(&mut s, &f.ret_ty);
            f.cfg
                .iter()
                .map(|x| x.get_all_types())
                .flatten()
                .for_each(|x| add_type(&mut s, &x));
        }
        s
    }

    pub fn export_to_c(&self, f: &mut std::fs::File) {
        let mut exporter = ExportC::default();
        exporter.export(self);
        writeln!(f, "{}", exporter.prelude).unwrap();
        writeln!(f, "{}", exporter.types).unwrap();
        writeln!(f, "{}", exporter.funs).unwrap();
    }
}
