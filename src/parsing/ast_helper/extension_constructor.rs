use crate::parsing::{
    ast_helper::{get_default_loc, Str},
    asttypes::MutableFlag,
    docstring::{add_info_attrs, get_empty_info, Info},
    location::Location,
    parsetree::{Attributes, ExtensionConstructor, ExtensionConstructorKind},
};

impl ExtensionConstructor {
    pub fn mk(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        infos: Option<Info>,
        mut_flag: Option<MutableFlag>,
        name: Str,
        kind: ExtensionConstructorKind,
    ) -> Self {
        let loc = loc.unwrap_or_else(get_default_loc);
        let mut attributes = attrs.unwrap_or_default();
        add_info_attrs(infos.unwrap_or_else(get_empty_info), &mut attributes);
        let mutable = mut_flag.unwrap_or(MutableFlag::Immutable);
        Self {
            name,
            kind,
            attributes,
            loc,
        }
    }
}
