use llvm::core::*;
use llvm::prelude::*;
use crate::*;

llvm_singleton! {
	singleton context: LLVMContextRef;
	create() = LLVMContextCreate();
	drop = LLVMContextDispose;
}
