use std::ffi::CString;
use llvm::core::*;
use llvm::prelude::*;
use crate::*;

pub fn transpile_statements_into_llvm(stmts: &mut [Stmt]) {
	let stmts_ref = &*stmts as *const [Stmt];
	for tuple in Type::tuple_list() {
        tuple.set_body()
    }
	for stmt in stmts.iter_mut() {
		match stmt {
			Stmt::ExternFun(_) => { /* already done; ignore */ },
			Stmt::TypeDef(typedef) => create_typedef(unsafe { &*stmts_ref }, typedef.baked().unwrap(), ""),
			Stmt::FunDef(fundef) => create_fundef(unsafe { &*stmts_ref }, fundef)
		}
	}
}

fn create_fundef(stmts: &[Stmt], fundef: &mut FunDef) {
	let FunDef { name, body, llvm_fun, ret_ty, vals, .. } = fundef;

	let fun = llvm_fun.unwrap();

	let ret_ty = ret_ty.as_determined();

	let bb = unsafe { LLVMAppendBasicBlockInContext(llvm_context(), fun, b"entry\0".as_ptr() as *const _) };
	unsafe { LLVMPositionBuilderAtEnd(llvm_builder(), bb) }

	let body = match body {
		FunBody::Baked(exprs) => exprs,
		FunBody::Builtin(builtin) => {
			match builtin {
				BuiltinFunBody::VariantFieldConstructor { mother_ty, variant_index } => {
					let mother_loc = mother_ty.as_scalar_loc().unwrap();
					let typedef = mother_loc.type_def().unwrap();
					create_variant_constructor(fun, typedef, mother_loc, *variant_index, name, stmts);
					unsafe { LLVMBuildRetVoid(llvm_builder()); }
				}
			}
			return
		}
		_ => unreachable!()
	};

	let terminated = transpile_complex_body(body, vals, stmts, name, false).0;

	if !terminated {
		if *ret_ty == Type::UNIT_TUPLE {
			unsafe { LLVMBuildRetVoid(llvm_builder()); }
		} else {
			panic!("return statement is missing from function `{name}`")
		}
	}
}

fn create_variant_constructor(
	fun: LLVMValueRef,
	typedef: &TypeDef,
	mother_loc: &TypeKindScalarLocation,
	variant_idx: usize,
	name: &str,
	stmts: &[Stmt]
) {
	let variant = &typedef.variants[variant_idx];

	let fields = match &variant.data {
		TypeVariantAttachedData::None => 0,
		TypeVariantAttachedData::Tuple(tuple) => Tuple::get(match &tuple.kind {
			TypeKind::Tuple { index } => *index,
			_ => unimplemented!("unreachable")
		}).len(),
		TypeVariantAttachedData::Struct { fields, .. } => fields.len()
	};
	let mut me = unsafe { LLVMGetLastParam(fun) };
	if typedef.variants.len() > 1 {
		// `me` is type { i8, [<num> x i8] }
		//                ^^ tag
		//                     ^^^^^^^^^^ amount of bytes enough to fit biggest field

		let tag = unsafe { LLVMBuildStructGEP(llvm_builder(), me, 0, b"\0".as_ptr() as _) };
		let tag_type = unsafe { LLVMInt8TypeInContext(llvm_context()) };
		let variant_tag_num = unsafe { LLVMConstInt(tag_type, variant_idx as _, 0) };
		unsafe { LLVMBuildStore(llvm_builder(), variant_tag_num, tag); }

		let dest_ty = match &variant.data {
			// We've set the tag already so simply return
			TypeVariantAttachedData::None => return,
			TypeVariantAttachedData::Tuple(tuple) => Type::tuple_list()[match &tuple.kind {
				TypeKind::Tuple { index } => *index,
				_ => unimplemented!("unreachable")
			}].llvm_type,
			TypeVariantAttachedData::Struct { llvm_type, .. } => llvm_type.unwrap()
		};
		let dest_ty = unsafe { LLVMPointerType(dest_ty, 0) };

		me = unsafe { LLVMBuildStructGEP(llvm_builder(), me, 1, b"\0".as_ptr() as _) };
		me = unsafe { LLVMBuildBitCast(llvm_builder(), me, dest_ty, b"\0".as_ptr() as _) }
	}
	for field in 0..fields {
		unsafe {
			let address = LLVMBuildStructGEP(llvm_builder(), me, field as _, b"\0".as_ptr() as _);
			let param = Expr {
				kind: ExprKind::Variable {
					location: ExprKindVariableLocation::FunArg {
						fun: FunLocation::Method(FunMethodLocation {
							ty_loc: mother_loc.clone(),
							method_index: typedef.methods.iter().enumerate().find(|(_, x)| x.def.name == *name).unwrap().0,
						}),
						var_index: field,
					},
				},
				// Stub
				ty: Type::UNIT_TUPLE,
			}.to_llvm_value(stmts, &name, &mut vec![]).0;
			let is_simplistic = match &variant.data {
				TypeVariantAttachedData::None => unreachable!(),
				TypeVariantAttachedData::Tuple(tuple) => Tuple::get(match &tuple.kind {
					TypeKind::Tuple { index } => *index,
					_ => unimplemented!("unreachable")
				})[field].is_simplistic(),
				TypeVariantAttachedData::Struct { fields, .. } => fields[field].ty.is_simplistic()
			};
			let param = if !is_simplistic {
				LLVMBuildLoad(llvm_builder(), param, b"\0".as_ptr() as _)
			} else {
				param
			};
			LLVMBuildStore(llvm_builder(), param, address);
		}
	}
}

fn create_typedef(stmts: &[Stmt], baked: &mut BakedType, prefix: &str) {
	// At this point `bake_types()` has already created the type name
	// but has not done the rest, so that's what we gonna do here

	match &mut baked.kind {
		BakedTypeKind::Ordinary(typedef) => {
			let subtypes_prefix = if prefix.is_empty() {
				typedef.name.clone()
			} else {
				format!("{prefix}.{}", typedef.name)
			};

			unsafe {
				if typedef.variants.len() != 1 {
					let byte = LLVMInt8TypeInContext(llvm_context());
					let biggest_field_size = typedef.variants.iter_mut().map(|x| match &mut x.data {
						TypeVariantAttachedData::None => 0,
						TypeVariantAttachedData::Struct { fields, llvm_type } => {
							let name = format!(".E.{}.{}\0", subtypes_prefix, x.name);
							let ty = LLVMStructCreateNamed(llvm_context(), name.as_ptr() as _);
							let mut elems = fields.iter().map(|x| x.ty.llvm_type(false)).collect::<Vec<_>>();
							LLVMStructSetBody(ty, elems.as_mut_ptr(), elems.len() as _, 0);
							*llvm_type = Some(ty);
							// TODO: Replace with size without padding bytes
							Type::size_of_llvm_type(ty)
						},
						// TODO: Replace with size without padding bytes
						TypeVariantAttachedData::Tuple(tuple) => Type::size_of_llvm_type(tuple.llvm_type(false))
					}).max().unwrap();
					let tag = byte;
					let mut elems = vec![tag];
					elems.push(LLVMArrayType(byte, biggest_field_size as _));
					LLVMStructSetBody(baked.llvm_type, elems.as_mut_ptr(), elems.len() as _, 0);
				} else {
					let mut elems = match &mut typedef.variants[0].data {
						TypeVariantAttachedData::None => vec![],
						TypeVariantAttachedData::Tuple(ty) => match &ty.kind {
							TypeKind::Tuple { index } => Tuple::get(*index).iter().map(|x| x.llvm_type(false)).collect(),
							_ => unreachable!()
						},
						TypeVariantAttachedData::Struct { fields, llvm_type } => {
							*llvm_type = Some(baked.llvm_type);
							fields.iter().map(|x| x.ty.llvm_type(false)).collect()
						}
					};
					LLVMStructSetBody(baked.llvm_type, elems.as_mut_ptr(), elems.len() as _, 0)
				}
			}

			let baked = match &mut typedef.subtypes {
                TypeList::Baked(baked) => baked,
                _ => unreachable!()
            };
			for ty in baked {
				create_typedef(stmts, ty, &subtypes_prefix)
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
