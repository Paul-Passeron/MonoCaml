use crate::parsing::{
    ast_helper::{get_default_loc, LId, Str},
    docstring::{add_info_attrs, get_empty_info, Info},
    location::Location,
    parsetree::{
        Attributes, ConstructorArguments, CoreType, ExtensionConstructor, ExtensionConstructorKind,
    },
};

impl ExtensionConstructor {
    pub fn mk(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        infos: Option<Info>,
        name: Str,
        kind: ExtensionConstructorKind,
    ) -> Self {
        let loc = loc.unwrap_or_else(get_default_loc);
        let mut attributes = attrs.unwrap_or_default();
        add_info_attrs(infos.unwrap_or_else(get_empty_info), &mut attributes);
        Self {
            name,
            kind,
            attributes,
            loc,
        }
    }

    pub fn decl(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        infos: Option<Info>,
        vars: Option<Vec<Str>>,
        args: Option<ConstructorArguments>,
        res: Option<CoreType>,
        name: Str,
    ) -> Self {
        let loc = loc.unwrap_or_else(get_default_loc);
        let mut attributes = attrs.unwrap_or_default();
        add_info_attrs(infos.unwrap_or_else(get_empty_info), &mut attributes);
        let vars = vars.unwrap_or_default();
        let args = args.unwrap_or(ConstructorArguments::Tuple(vec![]));
        Self {
            name,
            kind: ExtensionConstructorKind::Decl(vars, args, res),
            attributes,
            loc,
        }
    }

    pub fn rebind(
        loc: Option<Location>,
        attrs: Option<Attributes>,
        info: Option<Info>,
        name: Str,
        lid: LId,
    ) -> Self {
        let loc = loc.unwrap_or_else(get_default_loc);
        let mut attributes = attrs.unwrap_or_default();
        add_info_attrs(info.unwrap_or_else(get_empty_info), &mut attributes);
        Self {
            name,
            kind: ExtensionConstructorKind::Rebind(lid),
            attributes,
            loc,
        }
    }
}
