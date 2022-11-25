use std::ffi::CString;
use llvm::analysis::*;
use llvm::core::*;
use llvm::prelude::*;
use crate::*;

pub fn transpile_statements_into_llvm(stmts: &mut [Stmt]) {
	let stmts_ref = &*stmts as *const [Stmt];
	for stmt in stmts {
		match stmt {
			Stmt::ExternFun(fun) => create_extern_fun(fun),
			Stmt::TypeDef(typedef) => create_typedef(typedef),
			Stmt::FunDef(fundef) => create_fundef(unsafe { &*stmts_ref }, fundef),

			Stmt::Stub => unreachable!()
		}
	}
}

fn create_fundef(stmts: &[Stmt], fundef: &mut FunDef) {
	let FunDef { overloads, name } = fundef;
	for FunDefOverloadablePart { body, llvm_fun, ret_ty, vals, .. } in overloads {
		let body = match body {
			FunBody::Baked(exprs) => exprs,
			_ => unreachable!()
		};

		let ret_ty = ret_ty.as_determined();

		let fun = llvm_fun.unwrap();

		let bb = unsafe { LLVMAppendBasicBlockInContext(llvm_context(), fun, b"entry\0".as_ptr() as *const _) };
		unsafe { LLVMPositionBuilderAtEnd(llvm_builder(), bb) }

		let mut terminated = false;

		for i in 0..body.len() {
			match &body[i] {
				FunStmt::Return(expr) => {
					if expr.ty == Type::UNIT_TUPLE {
						// This will insert function calls and whatever stuff
						expr.kind.to_llvm_value(stmts);
						unsafe { LLVMBuildRetVoid(llvm_builder()); }
					} else {
						unsafe { LLVMBuildRet(llvm_builder(), expr.kind.to_llvm_value(stmts)); }
					}
					if i + 1 < body.len() {
						println!("WARNING: some statements in function `{name}` are unreachable");
						body.drain(i + 1..);
					}
					terminated = true;
				},
				FunStmt::Expr(expr) => match expr {
					// This builds function call and then just drops the value
					ExprKind::FunCall { .. } => drop(expr.to_llvm_value(stmts)),
					ExprKind::Variable(_) => panic!("variable is not allowed as a function statement"),
					ExprKind::Tuple(_) => panic!("tuple is not allowed as a function statement")
				},
				FunStmt::ValDef { line } => {
					let v = vals.get_mut(line).unwrap();
					v.llvm_value = Some(v.init.kind.to_llvm_value(stmts))
				}
			}
		}

		if !terminated {
			if *ret_ty == Type::UNIT_TUPLE {
				unsafe { LLVMBuildRetVoid(llvm_builder()); }
			} else {
				panic!("return statement is missing from function `{name}`")
			}
		}

		unsafe { LLVMVerifyFunction(fun, LLVMVerifierFailureAction::LLVMAbortProcessAction); }
	}
}

fn create_typedef(typedef: &TypeDefIndex) {
	// At this point `bake_types()` has already created the type name
	// but has not set the body, so that's what we gonna do here

	let baked = match Type::type_list() {
		TypeList::Baked(baked) => &baked[typedef.index],
		_ => unreachable!()
	};

	match &baked.kind {
		BakedTypeKind::Ordinary(typedef) => match &typedef.kind {
			TypeDefKind::Struct { fields } => {
				let mut fields = fields.iter().map(|x| x.ty.llvm_type()).collect::<Vec <_>>();
				unsafe { LLVMStructSetBody(baked.llvm_type, fields.as_mut_ptr(), fields.len() as _, 0) }
			},
			TypeDefKind::Opaque => { /* ignore */ }
			_ => todo!()
		},
		BakedTypeKind::Builtin(_) => { /* ignore */ }
	}
}

fn create_extern_fun(fun: &ExternFun) {
	create_llvm_fun(&fun.name, fun.args.iter().map(|ty| ty.llvm_type()).collect::<Vec <_>>(), &fun.ret_ty);
}

pub fn create_llvm_fun(name: &str, mut args: Vec <LLVMTypeRef>, ret_ty: &Type) -> LLVMValueRef {
	let ret_ty = if *ret_ty == Type::UNIT_TUPLE {
		unsafe { LLVMVoidTypeInContext(llvm_context()) }
	} else {
		ret_ty.llvm_type()
	};
	let fun_type = unsafe { LLVMFunctionType(ret_ty, args.as_mut_ptr(), args.len() as _, 0) };
	let name = CString::new(name).unwrap();
	unsafe { LLVMAddFunction(llvm_module(), name.as_ptr(), fun_type) }
}
