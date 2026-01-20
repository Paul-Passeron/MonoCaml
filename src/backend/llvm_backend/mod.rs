use crate::backend::Backend;
use crate::backend::llvm_backend::implem::LLVMBackendImpl;
use crate::cfg::Program;
use inkwell::OptimizationLevel;
use inkwell::context::Context;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use std::fs::File;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::env::temp_dir;

mod implem;

pub struct LLVMBackend {
    output_path: PathBuf,
}

impl Backend for LLVMBackend {
    type Out = Context;
    type Err = ();

    fn compile(self, program: &Program) -> Result<Context, ()> {
        let context = Context::create();
        {
            Target::initialize_all(&InitializationConfig::default());
            let mut backend = LLVMBackendImpl::new(&context);
            backend.compile(program)?;
            backend
                .module
                .verify()
                .map_err(|x| println!("{}", x.to_str().unwrap()))
                .unwrap();
            backend.mpm.run_on(&backend.module);
            for function in backend.module.get_functions() {
                backend.fpm.run_on(&function);
            }

            println!("{}", backend.module.to_string());
            let triple = TargetMachine::get_default_triple();
            let target = Target::from_triple(&triple).unwrap();
            let target_machine = target
                .create_target_machine(
                    &triple,
                    "generic", // CPU
                    "",        // Features
                    OptimizationLevel::Aggressive,
                    RelocMode::PIC,
                    CodeModel::Default,
                )
                .expect("Failed to create target machine");
            let dir = temp_dir();
            let p = dir.as_path().join("temp.o");
            let _ = File::create(&p).unwrap();
            target_machine
                .write_to_file(&backend.module, FileType::Object, &p)
                .unwrap();
            let _ = Command::new("cc")
                .arg(p)
                .arg("-o")
                .arg(self.output_path)
                .output()
                .map_err(|_| format!("Failed to compile runtime library"))
                .unwrap();
        }

        Ok(context)
    }
}

impl LLVMBackend {
    pub fn new<P: AsRef<Path>>(output_path: P) -> Self {
        Self {
            output_path: output_path.as_ref().to_path_buf(),
        }
    }
}
