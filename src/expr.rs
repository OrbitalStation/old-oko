use llvm::core::*;
use llvm::prelude::*;
use crate::*;
use core::fmt::{Debug, Formatter, Result};
use std::collections::HashMap;
use llvm::LLVMIntPredicate;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunMethodLocation {
	pub ty_loc: TypeKindScalarLocation,
	pub method_index: usize
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum FunLocation {
	Global { stmt_index: usize },
	Method(FunMethodLocation)
}

impl FunLocation {
	pub fn mother_typedef(&self) -> &'static mut TypeDef {
		match self {
			Self::Method(loc) => loc.ty_loc.type_def().unwrap(),
			_ => unimplemented!()
		}
	}

	pub fn method <'a> (&self) -> &'a mut AssociatedMethod {
		match self {
			Self::Method(loc) => &mut self.mother_typedef().methods[loc.method_index],
			_ => unimplemented!()
		}
	}

	pub fn fun <'a> (&self, stmts: &'a [Stmt]) -> &'a FunDef {
		match self {
			Self::Global { stmt_index, .. } => match &stmts[*stmt_index] {
				Stmt::FunDef(def) => def,
				_ => unreachable!()
			},
			Self::Method(_) => &self.method().def
		}
	}

	pub fn fun_mut <'a> (&self, stmts: &'a mut [Stmt]) -> &'a mut FunDef {
		match self {
			Self::Global { stmt_index, .. } => match &mut stmts[*stmt_index] {
				Stmt::FunDef(def) => def,
				_ => unreachable!()
			},
			Self::Method(_) => &mut self.method().def
		}
	}
}

#[derive(Debug, Clone)]
pub enum ExprKindVariableLocation {
	FunArg {
		fun: FunLocation,
		var_index: usize
	},
	Val {
		fun: FunLocation,
		line_def: usize
	},
	AccessField {
		i: Box <Expr>,
		def: &'static Vec <StructField>,
		field: usize
	},
	IInMethod {
		method: FunMethodLocation
	}
}

#[derive(Debug, Clone)]
pub enum VariableState {
	Scalar(VariableStateScalar),
	Struct(VariableStateStruct)
}

impl VariableState {
	pub fn valid(ty: &Type) -> Self {
		if let Some(fields) = ty.get_fields_of_struct() {
			Self::new_struct(fields)
		} else {
			VariableState::Scalar(VariableStateScalar::Valid)
		}
	}

	pub fn new_struct(fields: &Vec <StructField>) -> Self {
		VariableState::Struct(VariableStateStruct {
			fields_states: fields.iter().map(|f| VariableState::valid(&f.ty)).collect()
		})
	}

	pub fn is_fully_valid(&self) -> bool {
		match self {
			Self::Scalar(scalar) => *scalar == VariableStateScalar::Valid,
			Self::Struct(s) => s.fields_states.iter().find(|x| !x.is_fully_valid()).is_none()
		}
	}
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
#[repr(u8)]
pub enum VariableStateScalar {
	Moved,
	Valid
}

#[derive(Debug, Clone)]
pub struct VariableStateStruct {
	pub fields_states: Vec <VariableState>
}

#[derive(Copy, Clone, Eq, PartialEq)]
#[repr(u8)]
pub enum BinOpType {
	Add,
	Sub,
	Mul,
	Div,

	Eq,
	NotEq,

	And,
	Or
}

impl Debug for BinOpType {
	fn fmt(&self, f: &mut Formatter <'_>) -> Result {
		f.write_str(match self {
			Self::Add => "+",
			Self::Sub => "-",
			Self::Mul => "*",
			Self::Div => "/",
			Self::Eq => "==",
			Self::NotEq => "!=",
			Self::And => "and",
			Self::Or => "or"
		})
	}
}

#[derive(Debug, Clone, Eq, PartialEq)]
#[repr(u8)]
pub enum ExprLiteral {
	Integer(i128)
}

#[derive(Debug, Clone)]
#[repr(u8)]
pub enum ExprKind {
	Variable {
		location: ExprKindVariableLocation
	},
	Tuple(Vec <Expr>),
	Literal(ExprLiteral),
	FunCall {
		fun: FunLocation,
		args: Vec <Expr>
	},
	ExternFunCall {
		fun_stmt_index: usize,
		args: Vec <Expr>
	},
	BinOp {
		left: Box <Expr>,
		right: Box <Expr>,
		op: BinOpType
	},
	If {
		cond: Box <Expr>,
		yes: Vec <FunStmt>,
		no: Vec <FunStmt>
	},
	Dereference {
		ptr: Box <Expr>,
		may_be_mutable: bool
	}
}

impl ExprKind {
	pub const UNIT_TUPLE: ExprKind = ExprKind::Tuple(vec![]);
}

fn build_literal(lit: &ExprLiteral, ty: &Type) -> LLVMValueRef {
	unsafe {
		match lit {
			ExprLiteral::Integer(int) => LLVMConstInt(ty.llvm_type(false), core::mem::transmute(*int as i64), (*int > 0) as _)
		}
	}
}

fn build_bin_op(left0: &Expr, right0: &Expr, op: BinOpType, stmts: &[Stmt], fun_name: &str) -> LLVMValueRef {
	let left = left0.to_llvm_value(stmts, fun_name).0;
	let right = right0.to_llvm_value(stmts, fun_name).0;
	let name = b"\0".as_ptr() as _;
	unsafe {
		match op {
			BinOpType::Add => LLVMBuildAdd(llvm_builder(), left, right, name),
			BinOpType::Sub => LLVMBuildSub(llvm_builder(), left, right, name),
			BinOpType::Mul => LLVMBuildMul(llvm_builder(), left, right, name),
			BinOpType::Div => if left0.ty.is_signed() {
				LLVMBuildSDiv(llvm_builder(), left, right, name)
			} else if left0.ty.is_unsigned() {
				LLVMBuildUDiv(llvm_builder(), left, right, name)
			} else {
				panic!("bad operand for division")
			},
			BinOpType::Eq => LLVMBuildICmp(llvm_builder(), LLVMIntPredicate::LLVMIntEQ, left, right, name),
			BinOpType::NotEq => LLVMBuildICmp(llvm_builder(), LLVMIntPredicate::LLVMIntNE, left, right, name),
			BinOpType::And => LLVMBuildAnd(llvm_builder(), left, right, name),
			BinOpType::Or => LLVMBuildOr(llvm_builder(), left, right, name)
		}
	}
}

fn build_tuple(values: &Vec <Expr>, stmts: &[Stmt], fun_name: &str) -> LLVMValueRef {
	unsafe {
		let tuple_type = match Type::meet_new_tuple(values.iter().map(|x| x.ty.clone()).collect()).kind {
			TypeKind::Tuple { index } => Type::tuple_list()[index].llvm_type,
			_ => unreachable!()
		};
		let tuple =  LLVMBuildAlloca(llvm_builder(), tuple_type, b"\0".as_ptr() as _);
		for (idx, value) in values.into_iter().enumerate() {
			let elem = LLVMBuildStructGEP(llvm_builder(), tuple, idx as _, b"\0".as_ptr() as _);
			LLVMBuildStore(llvm_builder(), value.to_llvm_value(stmts, fun_name).0, elem);
		}
		tuple
	}
}

fn build_variable(location: &ExprKindVariableLocation, stmts: &[Stmt], is_lvalue: bool, fun_name: &str) -> LLVMValueRef {
	match location {
		ExprKindVariableLocation::FunArg {
			fun,
			var_index
		} => {
			if is_lvalue {
				panic!("fun args cannot be mutable; expected an lvalue")
			}
			let is_method = matches!(fun, FunLocation::Method(_));
			let fun = fun.fun(stmts);
			unsafe {
				LLVMGetParam(fun.llvm_fun.unwrap(), if is_method {
					// `i` goes as a first argument
					*var_index + 1
				} else {
					*var_index
				} as _)
			}
		},
		ExprKindVariableLocation::Val {
			fun,
			line_def
		} => {
			let fun = fun.fun(stmts);
			let val = fun.vals.get(line_def).unwrap();
			let llvm_value = val.llvm_value.unwrap();

			if !val.init.ty.is_simplistic() {
				llvm_value
			} else if val.mutable && !is_lvalue {
				unsafe { LLVMBuildLoad(llvm_builder(), llvm_value, b"\0".as_ptr() as _) }
			} else if !val.mutable && is_lvalue {
				panic!("expected an lvalue")
			} else {
				llvm_value
			}
		},
		ExprKindVariableLocation::AccessField {
			i,
			field,
			def
		} => {
			let ivalue = i.to_llvm_value(stmts, fun_name).0;
			let field_val = unsafe { LLVMBuildStructGEP(llvm_builder(), ivalue, *field as _, b"\0".as_ptr() as _) };
			if !def[*field].ty.is_simplistic() || is_lvalue {
				// It does not matter whether `is_lvalue` is set or not -
				// a non-simplistic type is passed by pointer anyway.

				// And if `is_lvalue` is set and the type is a copy one,
				// then we still do it by reference
				field_val
			} else {
				// Load a non-simplistic type that is not needed to be lvalue
				unsafe { LLVMBuildLoad(llvm_builder(), field_val, b"\0".as_ptr() as _) }
			}
		},
		ExprKindVariableLocation::IInMethod { method } => {
			let fun = FunLocation::Method(method.clone()).fun(stmts);
			unsafe { LLVMGetParam(fun.llvm_fun.unwrap(), 0) }
		}
	}
}

fn build_fun_call(fun: &FunLocation, args: &Vec <Expr>, stmts: &[Stmt], fun_name: &str) -> LLVMValueRef {
	let fun = fun.fun(stmts);
	let mut args = args.iter().map(|x| x.to_llvm_value(stmts, fun_name).0).collect::<Vec <_>>();
	if fun.is_ret_by_value() {
		unsafe { LLVMBuildCall(llvm_builder(), fun.llvm_fun.unwrap(), args.as_mut_ptr(), args.len() as _, b"\0".as_ptr() as _) }
	} else {
		let ret = unsafe { LLVMBuildAlloca(llvm_builder(), fun.ret_ty.as_determined().llvm_type(false), b"\0".as_ptr() as _) };
		args.push(ret);
		unsafe { LLVMBuildCall(llvm_builder(), fun.llvm_fun.unwrap(), args.as_mut_ptr(), args.len() as _, b"\0".as_ptr() as _) };
		ret
	}
}

fn build_extern_fun_call(fun_stmt_index: usize, args: &Vec <Expr>, stmts: &[Stmt], fun_name: &str) -> LLVMValueRef {
	let fun = match &stmts[fun_stmt_index] {
		Stmt::ExternFun(fun) => fun,
		_ => unreachable!()
	};
	let mut args = args.iter().map(|x| x.to_llvm_value(stmts, fun_name).0).collect::<Vec <_>>();
	unsafe { LLVMBuildCall(llvm_builder(), fun.llvm_fun.unwrap(), args.as_mut_ptr(), args.len() as _, b"\0".as_ptr() as _) }
}

fn build_if(cond: &Box <Expr>, yes: &Vec <FunStmt>, no: &Vec <FunStmt>, stmts: &[Stmt], fun_name: &str, ty: &Type) -> (LLVMValueRef, bool) {
	unsafe {
		let fun = LLVMGetBasicBlockParent(LLVMGetInsertBlock(llvm_builder()));
		let yes_bb = LLVMAppendBasicBlockInContext(llvm_context(), fun, b".yes\0".as_ptr() as _);
		let endif_bb = LLVMCreateBasicBlockInContext(llvm_context(), b".endif\0".as_ptr() as _);

		if !no.is_empty() {
			let no_bb = LLVMCreateBasicBlockInContext(llvm_context(), b".no\0".as_ptr() as _);

			LLVMBuildCondBr(llvm_builder(), cond.to_llvm_value(stmts, fun_name).0, yes_bb, no_bb);

			LLVMPositionBuilderAtEnd(llvm_builder(), yes_bb);
			let (yes_terminated, yes_val) = transpile_complex_body(yes, &mut HashMap::new(), stmts, fun_name, true);
			LLVMPositionBuilderAtEnd(llvm_builder(), yes_bb);
			if LLVMGetBasicBlockTerminator(yes_bb).is_null() {
				LLVMBuildBr(llvm_builder(), endif_bb);
			}

			LLVMAppendExistingBasicBlock(fun, no_bb);
			check_if_previous_basic_block_is_terminated_and_terminate_if_not(no_bb, endif_bb);

			LLVMPositionBuilderAtEnd(llvm_builder(), no_bb);
			let (no_terminated, no_val) = transpile_complex_body(no, &mut HashMap::new(), stmts, fun_name, true);
			LLVMPositionBuilderAtEnd(llvm_builder(), no_bb);
			if LLVMGetBasicBlockTerminator(no_bb).is_null() {
				LLVMBuildBr(llvm_builder(), endif_bb);
			}

			let are_both_branches_terminated = yes_terminated && no_terminated;

			if !are_both_branches_terminated {
				LLVMAppendExistingBasicBlock(fun, endif_bb);
				check_if_previous_basic_block_is_terminated_and_terminate_if_not(endif_bb, endif_bb);

				LLVMPositionBuilderAtEnd(llvm_builder(), endif_bb);
			}

			let result = if *ty != Type::UNIT_TUPLE {
				let phi = LLVMBuildPhi(llvm_builder(), ty.llvm_type(false), b"\0".as_ptr() as _);
				let mut values = vec![yes_val.unwrap(), no_val.unwrap()];
				let mut blocks = vec![yes_bb, no_bb];
				LLVMAddIncoming(phi, values.as_mut_ptr(), blocks.as_mut_ptr(), 2);
				phi
			} else {
				Expr::UNIT_TUPLE.to_llvm_value(stmts, fun_name).0
			};

			(result, are_both_branches_terminated)
		} else {
			LLVMBuildCondBr(llvm_builder(), cond.to_llvm_value(stmts, fun_name).0, yes_bb, endif_bb);

			LLVMPositionBuilderAtEnd(llvm_builder(), yes_bb);
			transpile_complex_body(yes, &mut HashMap::new(), stmts, fun_name, false);
			LLVMPositionBuilderAtEnd(llvm_builder(), yes_bb);
			if LLVMGetBasicBlockTerminator(yes_bb).is_null() {
				LLVMBuildBr(llvm_builder(), endif_bb);
			}

			LLVMAppendExistingBasicBlock(fun, endif_bb);
			check_if_previous_basic_block_is_terminated_and_terminate_if_not(endif_bb, endif_bb);
			LLVMPositionBuilderAtEnd(llvm_builder(), endif_bb);

			(Expr::UNIT_TUPLE.to_llvm_value(stmts, fun_name).0, false)
		}
	}
}

unsafe fn check_if_previous_basic_block_is_terminated_and_terminate_if_not(bb: LLVMBasicBlockRef, jmp: LLVMBasicBlockRef) {
	let prev_bb = LLVMGetPreviousBasicBlock(bb);
	if LLVMGetBasicBlockTerminator(prev_bb).is_null() {
		LLVMPositionBuilderAtEnd(llvm_builder(), prev_bb);
		LLVMBuildBr(llvm_builder(), jmp);
	}
}

fn build_dereference(ptr: &Expr, mutability: bool, stmts: &[Stmt], name: &str, is_lvalue: bool) -> LLVMValueRef {
	let llvm = ptr.to_llvm_value(stmts, name).0;
	if is_lvalue {
		assert!(mutability, "cannot mutate non-mutable dereference");
		llvm
	} else if let TypeKind::Pointer { ty, ..} | TypeKind::Reference { ty, ..} = &ptr.ty.kind {
		if ty.is_simplistic() {
			// Simplistic types are expected to be directly utilized
			unsafe { LLVMBuildLoad(llvm_builder(), llvm, b"\0".as_ptr() as _) }
		} else {
			llvm
		}
	} else {
		panic!("dereferencing of non-ptrs/refs is not currently allowed so that is a bug")
	}
}

#[derive(Debug, Clone)]
pub struct Expr {
	pub kind: ExprKind,
	pub ty: Type
}

impl Expr {
	pub const UNIT_TUPLE: Expr = Expr {
		kind: ExprKind::UNIT_TUPLE,
		ty: Type::UNIT_TUPLE
	};

	pub fn dereference_if_ref_or_nop(self) -> Self {
		let unaliased = self.ty.unaliasize();

		match &unaliased.kind {
			TypeKind::Reference { mutable, ty } => Self {
				kind: ExprKind::Dereference {
					ptr: Box::new(Self {
						kind: self.kind.clone(),
						ty: unaliased.clone()
					}),
					may_be_mutable: *mutable
				},
				ty: (**ty).clone(),
			},
			_ => self
		}
	}


	pub fn is_lvalue(&self, stmts: &[Stmt]) -> bool {
		match &self.kind {
			ExprKind::Variable { location } => match location {
				ExprKindVariableLocation::FunArg { .. } => false,
				ExprKindVariableLocation::Val { fun, line_def}
					=> fun.fun(stmts).vals.get(line_def).unwrap().mutable,
				ExprKindVariableLocation::AccessField { i, .. } => i.is_lvalue(stmts),
				ExprKindVariableLocation::IInMethod { method }
					=> matches!(FunLocation::Method(method.clone()).method().kind, AssociatedMethodKind::ByMutRef | AssociatedMethodKind::ByMutValue)
			},
			ExprKind::Dereference { may_be_mutable, .. } => *may_be_mutable,
			_ => false
		}
	}

	pub fn get_variable_state <'a> (&self, input: ParseFunBodyInput <'a>) -> &'a mut VariableState {
		match &self.kind {
			ExprKind::Variable { location } => match location {
				ExprKindVariableLocation::FunArg { fun, var_index }
					=> &mut fun.fun_mut(input.stmts_mut()).args[*var_index].state,
				ExprKindVariableLocation::Val { fun, line_def }
					=> &mut fun.fun_mut(input.stmts_mut()).vals.get_mut(line_def).unwrap().state,
				ExprKindVariableLocation::AccessField { i, field, def } => {
					let state = i.get_variable_state(input);
					match state {
						VariableState::Struct(struc) => {
							&mut struc.fields_states[*field]
						},
						VariableState::Scalar(x) => {
							assert_ne!(*x, VariableStateScalar::Moved, "variable already moved");
							*state = VariableState::new_struct(def);
							match state {
								VariableState::Struct(x) => &mut x.fields_states[*field],
								_ => unreachable!()
							}
						}
					}
				},
				ExprKindVariableLocation::IInMethod { method }
					=> &mut FunLocation::Method(method.clone()).method().state_of_i
			},
			ExprKind::Dereference { .. } => panic!("cannot move out of a reference"),
			_ => unreachable!()
		}
	}

	pub fn mark_as_moved_and_panic_if_already(&self, input: ParseFunBodyInput) {
		if self.ty.is_copy() {
			/* Values of only non-copy types can be moved */
			return
		}

		fn mark_state(state: &mut VariableState) {
			assert!(state.is_fully_valid(), "variable is already partially/fully moved");
			*state = VariableState::Scalar(VariableStateScalar::Moved)
		}

		match &self.kind {
			ExprKind::Variable { .. } => mark_state(self.get_variable_state(input)),
			ExprKind::Tuple(elems) | ExprKind::FunCall { args: elems, ..} | ExprKind::ExternFunCall { args: elems, .. } => for elem in elems {
				elem.mark_as_moved_and_panic_if_already(input)
			},
			ExprKind::Dereference { ptr, .. } => {
				match &ptr.ty.kind {
					TypeKind::Reference { ty, ..} | TypeKind::Pointer { ty, ..}
						=> assert!(ty.is_copy(), "cannot move out of a reference"),
					_ => unimplemented!("cannot do that yet :D")
				}
			},
			ExprKind::Literal(_) | ExprKind::BinOp { .. } | ExprKind::If { .. } => { /* ignore */ },
		}
	}

	fn _to_llvm(&self, stmts: &[Stmt], is_lvalue: bool, fun_name: &str) -> (LLVMValueRef, /* terminated */ bool) {
		(match &self.kind {
			ExprKind::Variable { location } => build_variable(location, stmts, is_lvalue, fun_name),
			ExprKind::Dereference { ptr, may_be_mutable } => build_dereference(ptr, *may_be_mutable, stmts, fun_name, is_lvalue),

			_ if is_lvalue => panic!("expected an lvalue"),

			ExprKind::Tuple(values) => build_tuple(values, stmts, fun_name),
			ExprKind::FunCall { fun, args }
				=> build_fun_call(fun, args, stmts, fun_name),
			ExprKind::ExternFunCall { fun_stmt_index, args }
				=> build_extern_fun_call(*fun_stmt_index, args, stmts, fun_name),
			ExprKind::BinOp { left, right, op }
				=> build_bin_op(left, right, *op, stmts, fun_name),
			ExprKind::Literal(lit) => build_literal(lit, &self.ty),
			ExprKind::If { cond, yes, no }
				=> return build_if(cond, yes, no, stmts, fun_name, &self.ty)
		}, false)
	}

	pub fn to_llvm_value(&self, stmts: &[Stmt], fun_name: &str) -> (LLVMValueRef, bool) {
		self._to_llvm(stmts, false, fun_name)
	}

	pub fn to_llvm_lvalue(&self, stmts: &[Stmt], fun_name: &str) -> (LLVMValueRef, bool) {
		self._to_llvm(stmts, true, fun_name)
	}
}
