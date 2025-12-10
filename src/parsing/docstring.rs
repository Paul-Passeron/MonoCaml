use crate::parsing::location::Location;

pub enum DSAttached {
    Unattached,
    Info,
    Docs,
}

pub enum DSAssociated {
    Zero,
    One,
    Many,
}

pub struct DocString<'a> {
    body: &'a str,
    loc: Location,
    attached: DSAttached,
    associated: DSAssociated,
}
