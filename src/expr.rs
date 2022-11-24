use llvm::core::*;
use llvm::prelude::*;
use crate::*;

#[derive(Debug, Clone)]
pub enum ExprKindVariableLocation {
	FunArg {
		fun_stmt_index: usize,
		fun_overload: usize,
		var_index: usize
	}
}

#[derive(Debug, Clone)]
#[repr(u8)]
pub enum ExprKind {
	Variable(ExprKindVariableLocation),
	Tuple(Vec <Expr>),
	Return(Box <Expr>),
	FunCall {
		fun_stmt_index: usize,
		fun_overload: usize,
		args: Vec <Expr>
	}
}

impl ExprKind {
	pub const UNIT_TUPLE: ExprKind = ExprKind::Tuple(vec![]);

	pub fn to_llvm_value(&self, stmts: &[Stmt]) -> LLVMValueRef {
		match self {
			Self::Variable(location) => match location {
				ExprKindVariableLocation::FunArg {
					fun_stmt_index,
					fun_overload,
					var_index
				} => {
					let fun = match &stmts[*fun_stmt_index] {
						Stmt::FunDef(fun) => fun,
						_ => unreachable!()
					};
					let overload = &fun.overloads[*fun_overload];
					unsafe {  LLVMGetParam(overload.llvm_fun.unwrap(), *var_index as _) }
				}
			},
			Self::Return(_) => panic!("cannot use `return` as part of an expression"),
			_ => todo!()
		}
	}
}

#[derive(Debug, Clone)]
pub struct Expr {
	pub kind: ExprKind,
	pub ty: Type
}

impl Expr {
	pub const UNIT_TUPLE: Expr = Expr {
		kind: ExprKind::UNIT_TUPLE,
		ty: Type::UNIT_TUPLE
	};

	pub fn ret(expr: Self) -> Self {
		Expr {
			kind: ExprKind::Return(Box::new(expr)),
			ty: Type::UNIT_TUPLE
		}
	}
}
