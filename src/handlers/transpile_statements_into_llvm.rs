use std::ffi::CString;
use llvm::core::*;
use crate::*;

pub fn transpile_statements_into_llvm(stmts: Vec <Stmt>) {
	for stmt in stmts {
		match stmt {
			Stmt::ExternFun(fun) => create_extern_fun(fun),
			Stmt::TypeDef(typedef) => create_typedef(typedef),
			_ => todo!()
		}
	}
}

fn create_typedef(typedef: TypeDefIndex) {
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

fn create_extern_fun(fun: ExternFun) {
	let mut args = fun.args.iter().map(|ty| ty.llvm_type()).collect::<Vec <_>>();
	let fun_type = unsafe {
		LLVMFunctionType(fun.ret_ty.llvm_type(), args.as_mut_ptr(), args.len() as _, 0)
	};
	let name = CString::new(fun.name).unwrap();
	let _ = unsafe { LLVMAddFunction(current_llvm_module(), name.as_ptr(), fun_type) };
}
