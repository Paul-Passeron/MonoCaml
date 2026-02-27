use crate::backend::Backend;
use crate::backend::llvm_backend::implem::LLVMBackendImpl;
use crate::cfg::Program;
use inkwell::OptimizationLevel;
use inkwell::context::Context;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::Builder;

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

            let triple = TargetMachine::get_default_triple();
            let target = Target::from_triple(&triple).unwrap();
            let target_machine = target
                .create_target_machine(
                    &triple,
                    "generic",
                    "",
                    OptimizationLevel::Aggressive,
                    RelocMode::PIC,
                    CodeModel::Default,
                )
                .expect("Failed to create target machine");

            let temp_file = Builder::new()
                .prefix("monocaml_")
                .suffix(".s")
                .tempfile()
                .expect("Failed to create temporary file");

            let temp_path = temp_file.into_temp_path();

            target_machine
                .write_to_file(&backend.module, FileType::Assembly, &temp_path)
                .unwrap();
            let out = Command::new("gcc")
                .arg(&temp_path)
                .arg("-o")
                .arg(&self.output_path)
                .arg("-no-pie")
                .output()
                .map_err(|_| "Failed to compile runtime library".to_string())
                .unwrap();

            if !out.status.success() {
                panic!(
                    "Failed to compile runtime library: {}",
                    String::from_utf8_lossy(&out.stderr)
                );
            }
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
