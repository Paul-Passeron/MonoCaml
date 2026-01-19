use crate::cfg::Program;

pub mod c_backend;
pub mod llvm_backend;

pub trait Backend {
    type Out;
    type Err;
    fn compile(self, prog: &Program) -> Result<Self::Out, Self::Err>;
}
