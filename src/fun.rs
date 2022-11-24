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
	pub is_simple: bool
}

impl FunDefOverloadablePart {
	pub const STUB: FunDefOverloadablePart = FunDefOverloadablePart {
		args: vec![],
		body: FunBody::Raw { lines: vec![] },
		ret_ty: FunRetType::Undetermined,
		is_simple: false
	};
}

#[derive(Debug, Clone)]
pub struct FunArg {
	pub name: String,
	pub ty: Type
}

#[derive(Debug, Clone)]
pub enum FunBody {
	Raw { lines: Vec <String> },
	Baked(Vec <ExprKind>)
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
