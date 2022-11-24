use llvm::core::*;
use llvm::prelude::*;

static mut LLVM_CONTEXT_REF: Option <LLVMContextRef> = None;

pub fn llvm_context() -> LLVMContextRef {
	unsafe { LLVM_CONTEXT_REF.unwrap() }
}

pub fn create_llvm_context() {
	drop_llvm_context();
	unsafe { LLVM_CONTEXT_REF = Some(LLVMContextCreate()) }
}

pub fn drop_llvm_context() {
	unsafe {
		if let Some(context) = LLVM_CONTEXT_REF {
			LLVMContextDispose(context)
		}
	}
}
