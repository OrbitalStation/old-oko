use llvm::prelude::*;
use crate::*;

#[derive(Debug)]
pub struct ExternFun {
	pub name: String,
	pub args: Vec <Type>,
	pub ret_ty: Type,
	pub llvm_fun: Option <LLVMValueRef>
}
