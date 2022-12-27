use std::collections::HashMap;
use llvm::core::*;
use llvm::prelude::*;
use crate::*;

#[derive(Debug, Clone)]
pub struct FunDef {
	pub name: String,
	pub args: Vec <FunArg>,
	pub body: FunBody,
	pub ret_ty: FunRetType,
	pub is_simple: bool,
	pub llvm_fun: Option <LLVMValueRef>,
	pub vals: HashMap <usize, VariableInfo>
}

impl FunDef {
	pub fn is_ret_by_value(&self) -> bool {
		self.ret_ty.as_determined().is_simplistic()
	}

	pub fn ret_ty_as_determined(&mut self, input: ParseFunBodyInput, method_info: Option <(&str, LLVMTypeRef, AssociatedMethodKind)>) -> &Type {
		if let FunRetType::Determined(ty) = unsafe { &*(&self.ret_ty as *const FunRetType) } {
			return ty
		}
		parse_fun_body(self, input, method_info);
		self.ret_ty.as_determined()
	}
}

#[derive(Debug, Clone)]
pub struct VariableInfo {
	pub name: String,
	pub init: Expr,
	pub mutable: bool,
	pub llvm_value: Option <LLVMValueRef>,
	pub state: VariableState
}

#[derive(Debug, Clone)]
pub struct FunArg {
	pub name: String,
	pub ty: Type,
	pub state: VariableState
}

impl FunArg {
	#[inline]
	pub fn is_by_value(&self) -> bool {
		self.ty.is_simplistic()
	}

	pub fn llvm_type(&self) -> LLVMTypeRef {
		let ty = self.ty.llvm_type();
		if self.is_by_value() {
			ty
		} else {
			unsafe { LLVMPointerType(ty, 0) }
		}
	}
}

#[derive(Debug, Clone)]
pub enum FunBody {
	Raw { code: String },
	Baked(Vec <FunStmt>)
}

#[derive(Debug, Clone)]
pub enum FunStmt {
	Expr(Expr),
	Return(Box <Expr>),
	ValDef { line: usize },
	Assignment { lvalue: Expr, new: Expr }
}

impl FunStmt {
	pub fn as_expr_mut(&mut self) -> Option <&mut Expr> {
		match self {
			Self::Expr(expr) => Some(expr),
			_ => None
		}
	}
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
