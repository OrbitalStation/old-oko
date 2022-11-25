use std::ffi::CString;
use llvm::analysis::*;
use llvm::core::*;
use llvm::prelude::*;
use crate::*;

pub fn transpile_statements_into_llvm(stmts: &[Stmt]) {
	for stmt in stmts {
		match stmt {
			Stmt::ExternFun(fun) => create_extern_fun(fun),
			Stmt::TypeDef(typedef) => create_typedef(typedef),
			Stmt::FunDef(fundef) => create_fundef(stmts, fundef),

			Stmt::Stub => unreachable!()
		}
	}
}

fn create_fundef(stmts: &[Stmt], fundef: &FunDef) {
	let FunDef { overloads, .. } = fundef;
	for FunDefOverloadablePart { body, llvm_fun, .. } in overloads {
		let body = match body {
			FunBody::Baked(exprs) => exprs,
			_ => unreachable!()
		};

		let fun = llvm_fun.unwrap();

		let bb = unsafe { LLVMAppendBasicBlockInContext(llvm_context(), fun, b"entry\0".as_ptr() as *const _) };
		unsafe { LLVMPositionBuilderAtEnd(llvm_builder(), bb) }

		for expr in body {
			match expr {
				ExprKind::Return(expr) => if expr.ty == Type::UNIT_TUPLE {
					unsafe { LLVMBuildRetVoid(llvm_builder()); }
				} else {
					unsafe { LLVMBuildRet(llvm_builder(), expr.kind.to_llvm_value(stmts)); }
				},
				_ => todo!()
			}
		}

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
