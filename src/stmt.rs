use crate::*;

#[derive(Debug)]
pub enum Stmt {
	TypeDef(TypeKindScalarLocation),
	FunDef(FunDef),
	ExternFun(ExternFun)
}
