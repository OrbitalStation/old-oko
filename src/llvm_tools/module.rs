use std::ffi::CString;
use llvm::core::*;
use llvm::prelude::*;
use crate::*;

llvm_singleton! {
	singleton module: LLVMModuleRef;
	create(name: &str) = {
		let mut name = CString::new(name).unwrap();
		LLVMModuleCreateWithName(name.as_ptr())
	};
	drop = LLVMDisposeModule;
}
