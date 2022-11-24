use crate::*;

#[derive(Debug)]
pub enum Stmt {
	/// Ignore it
	#[doc(hidden)]
	Stub,
	TypeDef(TypeDefIndex),
	FunDef(FunDef)
}
