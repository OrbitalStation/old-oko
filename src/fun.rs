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
	pub vals: Vec <VariableInfo>
}

impl FunDef {
	pub fn is_ret_by_value(&self) -> bool {
		self.ret_ty.as_determined().is_simplistic()
	}

	pub fn ret_ty_as_determined(
		&mut self,
		input: ParseFunBodyInput,
		method_info: Option <AssociatedMethodKind>,
		fun_loc: FunLocation
	) -> &Type {
		if let FunRetType::Determined(ty) = unsafe { &*(&self.ret_ty as *const FunRetType) } {
			return ty
		}
		let mut input_clone = (*input).clone();
		input_clone.mother_ty = match &fun_loc {
			FunLocation::Method(method) => Some(Type::from_kind(TypeKind::Scalar { loc: method.ty_loc.clone() })),
			_ => None
		};
		input_clone.fun_loc = fun_loc;
		parse_fun_body(self, &input_clone, method_info);
		self.ret_ty.as_determined()
	}
}

#[derive(Debug, Clone)]
pub struct VariableInfo {
	pub name: String,
	pub init: Expr,
	pub mutable: bool,
	pub llvm_value: Option <LLVMValueRef>,
	pub state: VariableState,
	pub idx_loc: Vec <usize>
}

impl VariableInfo {
	pub fn is_visible_from(&self, loc: &[usize]) -> bool {
		// ```
		// block
		// 	a := ...
		// 	block
		// 		b := ... <-- `loc` is here...
		// 		block
		// 			c := ...
		// 			block
		// 				d := ... <-- `self` is here
		// 			c1 := ... <-- ...or `loc` is here
		// 		b1 := ...
		// 	a1 := ...
		// ```
		// In both cases `self` is inaccessible
		if self.idx_loc.len() > loc.len() {
			return false
		}

		if self.idx_loc.len() == loc.len() {
			let is_in_same_block = self.idx_loc[..self.idx_loc.len() - 1] == loc[..loc.len() - 1];
			let is_further = self.idx_loc.last().unwrap() < loc.last().unwrap();
			return is_in_same_block && is_further
		}

		self.idx_loc.iter().zip(loc[..self.idx_loc.len()].iter()).all(|(x, y)| x <= y)
	}
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
		let ty = self.ty.llvm_type(false);
		if self.is_by_value() {
			ty
		} else {
			unsafe { LLVMPointerType(ty, 0) }
		}
	}
}

#[derive(Debug, Clone)]
#[repr(u8)]
pub enum BuiltinFunBody {
	VariantFieldConstructor {
		mother_ty: Type,
		variant_index: usize
	}
}

#[derive(Debug, Clone)]
pub enum FunBody {
	Raw { code: String },
	Baked(Vec <FunStmt>),
	Builtin(BuiltinFunBody)
}

#[derive(Debug, Clone)]
pub enum FunStmt {
	Expr(Expr),
	Return(Box <Expr>),
	ValDef { idx: usize },
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
