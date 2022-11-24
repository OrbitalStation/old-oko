use std::ffi::CString;
use llvm::core::*;
use llvm::prelude::*;

static mut LLVM_MODULE_REF: Option <LLVMModuleRef> = None;

pub fn current_llvm_module() -> LLVMModuleRef {
	unsafe { LLVM_MODULE_REF.unwrap() }
}

pub fn create_llvm_module(name: &str) {
	let name = CString::new(name).unwrap();
	unsafe { LLVM_MODULE_REF = Some(LLVMModuleCreateWithName(name.as_ptr())) }
}

pub fn drop_llvm_module() {
	unsafe {
		if let Some(module) = LLVM_MODULE_REF {
			LLVMDisposeModule(module)
		}
	}
}
