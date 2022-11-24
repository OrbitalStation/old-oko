use llvm::core::*;
use llvm::prelude::*;
use crate::*;

llvm_singleton! {
	singleton builder: LLVMBuilderRef;
	create() = LLVMCreateBuilderInContext(llvm_context());
	drop = LLVMDisposeBuilder;
}
