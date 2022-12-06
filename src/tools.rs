use std::collections::HashMap;
use crate::*;
use llvm::core::*;
use llvm::prelude::*;

pub fn handle_complex_body_line(body: &String, input: ParseFunBodyInput) -> (Vec <FunStmt>, Type) {
	let mut body: Vec <(FunStmt, Type)> = parse_complex_body(&body, input).unwrap();
	let (last, last_ty) = body.pop().unwrap();

	let vec = body.into_iter().map(|(stmt, ty)| {
		assert_eq!(ty, Type::UNIT_TUPLE, "a statement from a complex body is not of `()` type");
		stmt
	}).chain(core::iter::once(last)).collect();

	(vec, last_ty)
}

pub fn transpile_complex_body(body: &Vec <FunStmt>, vals: &mut HashMap <usize, VariableInfo>, stmts: &[Stmt], name: &str, get_last: bool) -> (bool, Option <LLVMValueRef>) {
	for i in 0..body.len() {
		let get_last = get_last && i + 1 == body.len();

		match &body[i] {
			FunStmt::Return(expr) => {
				if expr.ty == Type::UNIT_TUPLE {
					// This will insert function calls and whatever stuff
					expr.to_llvm_value(stmts, name);
					unsafe { LLVMBuildRetVoid(llvm_builder()); }
				} else {
					unsafe { LLVMBuildRet(llvm_builder(), expr.to_llvm_value(stmts, name).0); }
				};
				if i + 1 < body.len() {
					panic!("ERROR: some statements in function `{name}` are unreachable")
				}
				return (true, if get_last {
					Some(Expr::UNIT_TUPLE.to_llvm_value(stmts, name).0)
				} else {
					None
				})
			},
			FunStmt::Expr(expr) => match expr {
				ExprKind::FunCall { .. } | ExprKind::ExternFunCall { .. } => {
					let e = Expr {
						ty: Type::UNIT_TUPLE,
						kind: expr.clone()
					}.to_llvm_value(stmts, name).0;
					if get_last {
						return (false, Some(e))
					}
				},
				ExprKind::If { .. } => {
					let (e, k) = Expr {
						ty: Type::UNIT_TUPLE,
						kind: expr.clone()
					}.to_llvm_value(stmts, name);
					if k {
						if i + 1 < body.len() {
							panic!("ERROR: some statements in function `{name}` are unreachable")
						}
						return (true, if get_last {
							Some(Expr::UNIT_TUPLE.to_llvm_value(stmts, name).0)
						} else {
							None
						})
					}
					if get_last {
						return (false, Some(e))
					}
				},
				ExprKind::BinOp { .. } => panic!("binary operator is not allowed as a function statement"),
				ExprKind::Variable(_) => panic!("variable is not allowed as a function statement"),
				ExprKind::Tuple(_) => panic!("tuple is not allowed as a function statement"),
				ExprKind::Literal(_) => panic!("literal is not allowed as a function statement")
			},
			FunStmt::ValDef { line } => {
				let v = vals.get_mut(line).unwrap();
				if v.mutable {
					let llvm_value = unsafe { LLVMBuildAlloca(llvm_builder(), v.init.ty.llvm_type(), b"\0".as_ptr() as _) };
					unsafe { LLVMBuildStore(llvm_builder(), v.init.to_llvm_value(stmts, name).0, llvm_value) };
					v.llvm_value = Some(llvm_value)
				} else {
					v.llvm_value = Some(v.init.to_llvm_value(stmts, name).0)
				}
				if get_last {
					return (false, Some(unsafe { LLVMBuildLoad(llvm_builder(), v.llvm_value.unwrap(), b"\0".as_ptr() as _) }))
				}
			},
			FunStmt::Assignment { lvalue, new } => {
				unsafe { LLVMBuildStore(llvm_builder(), new.to_llvm_value(stmts, name).0, lvalue.to_llvm_lvalue(stmts, name).0); }
				if get_last {
					return (false, Some(Expr::UNIT_TUPLE.to_llvm_value(stmts, name).0))
				}
			}
		};
	}

	if !get_last {
		(false, None)
	} else {
		unimplemented!("you've encountered a bug")
	}
}
