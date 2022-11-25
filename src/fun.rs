use std::collections::HashMap;
use llvm::prelude::*;
use crate::*;

#[derive(Debug)]
pub struct FunDef {
	pub name: String,
	pub overloads: Vec <FunDefOverloadablePart>
}

#[derive(Debug, Clone)]
pub struct FunDefOverloadablePart {
	pub args: Vec <FunArg>,
	pub body: FunBody,
	pub ret_ty: FunRetType,
	pub is_simple: bool,
	pub llvm_fun: Option <LLVMValueRef>,
	pub vals: HashMap <usize, VariableInfo>
}

#[derive(Debug, Clone)]
pub struct VariableInfo {
	pub name: String,
	pub init: Expr,
	pub llvm_value: Option <LLVMValueRef>
}

#[derive(Debug, Clone)]
pub struct FunArg {
	pub name: String,
	pub ty: Type
}

#[derive(Debug, Clone)]
pub enum FunBody {
	Raw { lines: Vec <String> },
	Baked(Vec <FunStmt>)
}

#[derive(Debug, Clone)]
pub enum FunStmt {
	Expr(ExprKind),
	Return(Box <Expr>),
	ValDef { line: usize }
}

#[derive(Debug, Clone)]
pub enum FunRetType {
	Undetermined,
	Determined(Type)
}

impl FunRetType {
	pub fn as_determined(&self) -> &Type {
		match self {
			Self::Determined(ty) => ty,
			Self::Undetermined => unreachable!()
		}
	}
}
