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
		args: Vec <Expr>
	}
}

impl ExprKind {
	pub const UNIT_TUPLE: ExprKind = ExprKind::Tuple(vec![]);
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
