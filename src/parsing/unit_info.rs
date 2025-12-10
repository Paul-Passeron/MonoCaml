use std::path::Path;

use crate::{
    parsing::location::Location,
    utils::{self, misc},
};

#[derive(Clone, Copy)]
pub enum IntfOrImpl {
    Intf,
    Impl,
}

pub type ModName = String;
pub type Filename = String;
pub type FilePrefix = String;

pub enum Error {
    InvalidEncoding(String),
}

pub struct UnitInfo {
    source_file: Filename,
    prefix: FilePrefix,
    modname: ModName,
    kind: IntfOrImpl,
}

impl UnitInfo {
    pub fn source_file(&self) -> &Filename {
        &self.source_file
    }
    pub fn prefix(&self) -> &FilePrefix {
        &self.prefix
    }
    pub fn modname(&self) -> &ModName {
        &self.modname
    }
    pub fn kind(&self) -> IntfOrImpl {
        self.kind
    }

    pub fn basename_chop_extensions<'a>(basename: &'a str) -> &'a str {
        if let Some(index) = basename.rfind('.') {
            &basename[..index]
        } else {
            basename
        }
    }

    pub fn strict_modulize<'a>(s: &'a str) -> Result<String, Error> {
        match utils::misc::capitalize(s) {
            Ok(capitalized) => Ok(capitalized),
            Err(_) => Err(Error::InvalidEncoding(s.to_string())),
        }
    }

    pub fn modulize<'a>(s: &'a str) -> String {
        match utils::misc::capitalize(s) {
            Ok(capitalized) => capitalized,
            Err(x) => x,
        }
    }

    pub fn normalize<'a>(s: &'a str) -> String {
        match utils::misc::normalized_unit_filename(s) {
            Ok(capitalized) => capitalized,
            Err(x) => x,
        }
    }

    pub fn stem<'a>(source_file: &'a str) -> String {
        Self::basename_chop_extensions(
            Path::new(source_file)
                .normalize_lexically()
                .unwrap()
                .file_name()
                .unwrap()
                .to_str()
                .unwrap(),
        )
        .to_string()
    }

    pub fn strict_modname_from_source(source_file: &str) -> Result<String, Error> {
        Self::strict_modulize(&Self::stem(source_file))
    }

    pub fn lax_modname_from_source(source_file: &str) -> String {
        Self::modulize(&Self::stem(source_file))
    }

    pub fn is_unit_name(source: &str) -> bool {
        misc::is_valid_identifier(source)
    }

    pub fn check_unit_name(&self) {
        let modname = self.modname();
        if !(Self::is_unit_name(modname)) {
            eprintln!("TODO: report warning: Bad module name {}", modname)
            // (Location::in_file(self.source_file())).prerr_warning(Warnings::BadModuleName(modname))
        }
    }
}
