use std::ffi::CString;
use llvm::core::*;
use llvm::prelude::*;
use crate::*;

llvm_singleton! {
	singleton module: LLVMModuleRef;
	create(name: &str) = {
		let mut name = CString::new(name).unwrap();
		LLVMModuleCreateWithNameInContext(name.as_ptr(), llvm_context())
	};
	drop = LLVMDisposeModule;
}
