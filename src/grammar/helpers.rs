use crate::*;

#[repr(u8)]
#[derive(Debug)]
pub(in crate) enum AssociatedItem {
	Method(AssociatedMethod),
	/// Don't need to hold any data, everything is added automatically
	Type
}

pub(in crate) fn _check2arithmetic(mut x: Expr, mut y: Expr, op: BinOpType, ty: impl for <'a> FnOnce(&'a Expr) -> Type) -> Expr {
	assert!(x.ty.eq_implicit(&mut y.ty, Some(&mut x.kind), Some(&mut y.kind)), "cannot apply `{:?}` to different types", op);
	let ty = ty(&x);
	Expr {
		kind: ExprKind::BinOp {
			left: Box::new(x),
			right: Box::new(y),
			op,
		},
		ty
	}
}

pub(in crate) fn check2arithmetic(x: Expr, y: Expr, op: BinOpType) -> Expr {
	assert!(x.ty.is_arithmetic(), "cannot apply `{:?}` to non-arithmetic types", op);
	_check2arithmetic(x, y, op, |x| x.ty.clone())
}

pub(in crate) fn check2arithmetic_bool(x: Expr, y: Expr, op: BinOpType) -> Expr {
	_check2arithmetic(x, y, op, |_| Type::get_by_name("bool"))
}

pub(in crate) fn check2full_bool(x: Expr, y: Expr, op: BinOpType) -> Expr {
	let bool = Type::get_by_name("bool");
	assert_eq!(x.ty, bool, "cannot apply `{:?}` to non-boolean type", op);
	assert_eq!(y.ty, bool, "cannot apply `{:?}` to non-boolean type", op);
	_check2arithmetic(x, y, op, |_| bool)
}

pub(in crate) fn _get_static_method(mother: &Type, name: &str) -> Option <(FunLocation, &'static mut FunDef)> {
	let (ty_loc, method_index, method) = _get_fun_get_loc_and_method(&mother, name)?;
	assert_eq!(method.kind, AssociatedMethodKind::Static);
	Some((FunLocation::Method(FunMethodLocation {
		ty_loc,
		method_index,
	}), &mut method.def))
}

pub(in crate) fn access_field_inner(mother: Option <Result <Type, Expr>>, input: ParseFunBodyInput, mut fields: Vec <(String, Option <Vec <Expr>>)>) -> Option <Expr> {
	let mut cur = match mother {
		Some(mother) => match mother {
			Ok(mother) => {
				// Check for static associated items
				let (first_name, first_args) = fields.remove(0);
				// TODO: replace that `?` with check for static associated variables
				let (fun, def) = _get_static_method(&mother, &first_name)?;
				let ty = def.ret_ty_as_determined(input, Some(AssociatedMethodKind::Static), fun.clone()).clone();
				let args = first_args.unwrap_or(vec![]);
				assert_eq!(def.args.len(), args.len(), "function argument number mismatch");
				Expr {
					kind: ExprKind::FunCall {
						fun,
						args
					},
					ty
				}
			},
			Err(mother) => mother
		},
		None => {
			let (first_name, first_args) = fields.remove(0);

			let var = if first_args.is_some() {
				None
			} else {
				__expr1_variable(&first_name, input).ok()
			};

			match var {
				Some(x) => x,
				None => {
					let (stmt_index, fun) = input.fun_by_name_mut(&first_name)?;
					let args = match first_args {
						None => if !fun.args.is_empty() {
							return None
						} else {
							vec![]
						},
						Some(x) => x
					};
					assert_eq!(fun.args.len(), args.len(), "function args number mismatch");
					do_fun_call((FunLocation::Global { stmt_index }, fun, args.len(), None), args, input)
				}
			}
		}
	};

	for (next_name, next_args) in fields {
		cur = cur.dereference_if_ref_or_nop();
		if let Some((ty_loc, method_index, method)) = _get_fun_get_loc_and_method(&cur.ty, &next_name) {
			let args = next_args.unwrap_or(vec![]);
			assert_eq!(method.def.args.len(), args.len(), "function args number mismatch");
			let loc = FunLocation::Method(FunMethodLocation {
				ty_loc,
				method_index
			});
			cur = do_fun_call((loc, &mut method.def, args.len(), Some(cur)), args, input);
			continue
		}

		assert!(next_args.is_none(), "no function named `{next_name}` found in `{:?}`", cur.ty);

		let fields = cur.ty.get_fields_of_struct().expect("cannot access a field of not-a-structure");
		let (idx, field) = fields.iter().enumerate().find(|(_, x)| x.name == next_name).expect("no field with such a name found");
		cur = Expr {
			kind: ExprKind::Variable {
				location: ExprKindVariableLocation::AccessField {
					i: Box::new(cur),
					def: fields,
					field: idx
				}
			},
			ty: field.ty.clone()
		}
	}

	Some(cur)
}

pub(in crate) fn _get_fun_get_loc_and_method(ty: &Type, method: &str) -> Option <(TypeKindScalarLocation, usize, &'static mut AssociatedMethod)> {
	match &ty.kind {
		TypeKind::Scalar { loc } => {
			loc.type_def()?.methods.iter_mut().enumerate().find(|(_, x)| x.def.name == method).map(|(a, b)| (loc.clone(), a, b))
		},
		_ => None
	}
}

pub(in crate) fn get_fun(mother: Option <Result <Type, Expr>>, input: ParseFunBodyInput, mut components: Vec <(String, Option <Vec <Expr>>)>) -> Option <Result <(FunLocation, &mut FunDef, usize, Option <Expr>), Expr>> {
	Some(if components.len() == 1 {
		let (first_name, first_args) = components.remove(0);
		let i = if let Some(mother) = mother {
			match mother {
				// Static method
				Ok(mother) => {
					let (loc, method) = _get_static_method(&mother, &first_name)?;
					let len = method.args.len();
					(loc, method, len, None)
				},
				Err(expr) => {
					// Non-static method on `(expr)`
					let (ty_loc, method_index, method) = _get_fun_get_loc_and_method(&expr.ty, &first_name)?;
					let len = method.def.args.len();
					(FunLocation::Method(FunMethodLocation {
						ty_loc,
						method_index,
					}), &mut method.def, len, Some(expr))
				}
			}
		} else {
			// Global function
			let (i, d) = input.fun_by_name_mut(&first_name)?;
			let len = d.args.len();
			(FunLocation::Global { stmt_index: i }, d, len, None)
		};
		match first_args {
			// Not an in-place call
			None => Ok(i),
			// An in-place call
			Some(args) => Err(do_fun_call(i, args, input))
		}
	} else {
		let (fun_name, fun_args) = components.pop().unwrap();
		let mother_ty = access_field_inner(mother, input, components)?.dereference_if_ref_or_nop();
		let (ty_loc, method_index, method) = _get_fun_get_loc_and_method(&mother_ty.ty, &fun_name)?;
		let len = method.def.args.len();
		let i = (FunLocation::Method(FunMethodLocation {
			ty_loc,
			method_index,
		}), &mut method.def, len, Some(mother_ty));
		match fun_args {
			None => Ok(i),
			Some(args) => Err(do_fun_call(i, args, input))
		}
	})
}

pub(in crate) fn assignment(input: ParseFunBodyInput, mut lvalue: Expr, mut new: Expr) -> (FunStmt, Type) {
	// if !lvalue.ty.is_copy() {
	//     // TODO! DROP lvalue and do not forget that drop may occur only on fully valid values
	// }

	assert!(lvalue.is_lvalue(input.stmts()), "cannot assign to a non-lvalue");
	new.mark_as_moved_and_panic_if_already(input);
	assert!(new.ty.eq_implicit(&mut lvalue.ty, Some(&mut new.kind), Some(&mut lvalue.kind)), "cannot assign two different types");
	(FunStmt::Assignment { lvalue, new }, Type::UNIT_TUPLE)
}

pub(in crate) fn dereference(ptr: Expr) -> Expr {
	let (ty, mutability) = match &ptr.ty.kind {
		TypeKind::Reference { ty, mutable } => ((**ty).clone(), *mutable),
		TypeKind::Pointer { ty, ptrs } => if ptrs.len == 1 {
			((**ty).clone(), (ptrs.muts & 1) != 0)
		} else {
			(Type::from_kind(TypeKind::Pointer {
				ty: ty.clone(),
				ptrs: TypePointers {
					len: ptrs.len - 1,
					muts: ptrs.muts << 1
				}
			}), (ptrs.muts & 1) != 0)
		},
		_ => panic!("cannot dereference a type that is neither pointer nor reference")
	};
	Expr {
		kind: ExprKind::Dereference {
			ptr: Box::new(ptr),
			may_be_mutable: mutability
		},
		ty,
	}
}

pub(in crate) fn __non_ptr_ty_helper(ty: &Type) -> Result <TypeKindScalarLocation, Vec <String>> {
	match Type::type_list() {
		TypeList::Baked(_) => Ok(ty.as_scalar_loc().unwrap().clone()),
		TypeList::Raw(_) => Err(vec![])
	}
}

pub(in crate) fn __non_ptr_ty_cont_helper(loc: Result <TypeKindScalarLocation, Vec <String>>, new: String) -> Option <Result <TypeKindScalarLocation, Vec <String>>> {
	match loc {
		Ok(loc) => {
			match &loc.type_def().unwrap().subtypes {
				TypeList::Baked(baked) => Some(Ok(TypeKindScalarLocation::AssociatedItem {
					index: baked.iter().enumerate().find(|(_, x) | match &x.kind {
						BakedTypeKind::Ordinary(def) => def.name == new,
						BakedTypeKind::FullAlias { name, .. } => *name == new,
						BakedTypeKind::SeqAlias(_) | BakedTypeKind::Builtin(_) => unreachable!()
					})?.0,
					mother: Box::new(loc),
				})),
				TypeList::Raw(_) => unimplemented!()
			}
		},
		Err(mut x) => {
			x.push(new);
			Some(Err(x))
		}
	}
}

pub(crate) fn do_fun_call(i: (FunLocation, &mut FunDef, usize, Option <Expr>), mut args: Vec <Expr>, input: ParseFunBodyInput) -> Expr {
	let (fun_loc, fun_def, _, i_of_method) = i;

	assert_eq!(args.len(), fun_def.args.len(), "argument number doesn't match in call of `{}`", fun_def.name);

	for (idx, arg) in args.iter_mut().enumerate() {
		if !arg.ty.try_implicitly_convert(&fun_def.args[idx].ty, None) {
			panic!("argument types doesn't match in call of `{}`", fun_def.name)
		}
		arg.mark_as_moved_and_panic_if_already(input)
	}

	let method_info = if let Some(i) = i_of_method {
		args.insert(0, i);
		Some(fun_loc.method().kind)
	} else if matches!(fun_loc, FunLocation::Method(_)) {
		Some(fun_loc.method().kind)
	} else {
		None
	};

	Expr {
		kind: ExprKind::FunCall {
			fun: fun_loc.clone(),
			args
		},
		ty: fun_def.ret_ty_as_determined(input, method_info, fun_loc).clone()
	}
}
