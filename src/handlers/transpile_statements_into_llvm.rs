use std::ffi::CString;
use llvm::analysis::*;
use llvm::core::*;
use llvm::prelude::*;
use crate::*;

pub fn transpile_statements_into_llvm(stmts: &mut [Stmt]) {
	let stmts_ref = &*stmts as *const [Stmt];
	for tuple in Type::tuple_list() {
        let mut types = tuple.fields.iter().map(|x| x.llvm_type(false)).collect::<Vec <_>>();
		unsafe { LLVMStructSetBody(tuple.llvm_type, types.as_mut_ptr(), types.len() as _, 0) };
    }
	for stmt in stmts.iter_mut() {
		match stmt {
			Stmt::ExternFun(_) => { /* already done; ignore */ },
			Stmt::TypeDef(typedef) => create_typedef(unsafe { &*stmts_ref }, typedef.baked().unwrap()),
			Stmt::FunDef(fundef) => create_fundef(unsafe { &*stmts_ref }, fundef)
		}
	}
}

fn create_fundef(stmts: &[Stmt], fundef: &mut FunDef) {
	let FunDef { name, body, llvm_fun, ret_ty, vals, .. } = fundef;

	let body = match body {
		FunBody::Baked(exprs) => exprs,
		_ => unreachable!()
	};

	let ret_ty = ret_ty.as_determined();

	let fun = llvm_fun.unwrap();

	let bb = unsafe { LLVMAppendBasicBlockInContext(llvm_context(), fun, b"entry\0".as_ptr() as *const _) };
	unsafe { LLVMPositionBuilderAtEnd(llvm_builder(), bb) }

	let terminated = transpile_complex_body(body, vals, stmts, name, false).0;

	if !terminated {
		if *ret_ty == Type::UNIT_TUPLE {
			unsafe { LLVMBuildRetVoid(llvm_builder()); }
		} else {
			panic!("return statement is missing from function `{name}`")
		}
	}

	unsafe { LLVMVerifyFunction(fun, LLVMVerifierFailureAction::LLVMAbortProcessAction); }
}

fn create_typedef(stmts: &[Stmt], baked: &mut BakedType) {
	// At this point `bake_types()` has already created the type name
	// but has not set the body, so that's what we gonna do here

	match &mut baked.kind {
		BakedTypeKind::Ordinary(typedef) => {
			match &typedef.kind {
				TypeDefKind::Struct { fields } => {
					let mut fields = fields.iter().map(|x| x.ty.llvm_type(false)).collect::<Vec <_>>();
					unsafe { LLVMStructSetBody(baked.llvm_type, fields.as_mut_ptr(), fields.len() as _, 0) }
				},
				TypeDefKind::Opaque => { /* ignore */ }
				_ => todo!("Enum")
			}

			let baked = match &mut typedef.subtypes {
                TypeList::Baked(baked) => baked,
                _ => unreachable!()
            };
			for ty in baked {
				create_typedef(stmts, ty)
			}

			for method in &mut typedef.methods {
				create_fundef(stmts, &mut method.def)
			}
		},
		BakedTypeKind::Builtin(_) | BakedTypeKind::SeqAlias(_) | BakedTypeKind::FullAlias { .. } => { /* ignore */ }
	}
}

pub fn create_llvm_fun(name: &str, mut args: Vec <LLVMTypeRef>, ret_ty: &Type) -> LLVMValueRef {
	let ret_ty = if *ret_ty == Type::UNIT_TUPLE {
		unsafe { LLVMVoidTypeInContext(llvm_context()) }
	} else if ret_ty.is_simplistic() {
		ret_ty.llvm_type(false)
	} else {
		args.push(unsafe { LLVMPointerType(ret_ty.llvm_type(false), 0) });
		unsafe { LLVMVoidTypeInContext(llvm_context()) }
	};
	let fun_type = unsafe { LLVMFunctionType(ret_ty, args.as_mut_ptr(), args.len() as _, 0) };
	let name = CString::new(name).unwrap();
	unsafe { LLVMAddFunction(llvm_module(), name.as_ptr(), fun_type) }
}
