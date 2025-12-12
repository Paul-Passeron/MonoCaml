use crate::parsing::parsetree::{
    ClassField, ClassSignature, ClassStructure, ClassTypeField, CoreType, Pattern,
};

impl ClassSignature {
    pub fn mk(this: CoreType, fields: Vec<ClassTypeField>) -> Self {
        Self { this, fields }
    }
}

impl ClassStructure {
    pub fn mk(this: Pattern, fields: Vec<ClassField>) -> Self {
        Self { this, fields }
    }
}


