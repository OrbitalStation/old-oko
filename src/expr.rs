use llvm::core::*;
use llvm::prelude::*;
use crate::*;
use core::fmt::{Debug, Formatter, Result};
use std::collections::HashMap;
use llvm::LLVMIntPredicate;

#[derive(Debug, Clone)]
pub enum ExprKindVariableLocation {
	FunArg {
		fun_stmt_index: usize,
		fun_overload: usize,
		var_index: usize
	},
	Val {
		fun_stmt_index: usize,
		fun_overload: usize,
		line_def: usize
	}
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
}

impl Debug for BinOpType {
	fn fmt(&self, f: &mut Formatter <'_>) -> Result {
		f.write_str(match self {
			Self::Add => "+",
			Self::Sub => "-",
			Self::Mul => "*",
			Self::Div => "/",
			Self::Eq => "==",
			Self::NotEq => "!="
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
	Variable(ExprKindVariableLocation),
	Tuple(Vec <Expr>),
	Literal(ExprLiteral),
	FunCall {
		fun_stmt_index: usize,
		fun_overload: usize,
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
	}
}

impl ExprKind {
	pub const UNIT_TUPLE: ExprKind = ExprKind::Tuple(vec![]);
}

fn build_literal(lit: &ExprLiteral, ty: &Type) -> LLVMValueRef {
	match lit {
		ExprLiteral::Integer(int) => unsafe { LLVMConstInt(ty.llvm_type(), core::mem::transmute(*int as i64), (*int > 0) as _)}
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
			BinOpType::NotEq => LLVMBuildICmp(llvm_builder(), LLVMIntPredicate::LLVMIntNE, left, right, name)
		}
	}
}

fn build_tuple(values: &Vec <Expr>, stmts: &[Stmt], fun_name: &str) -> LLVMValueRef {
	// TODO! Rework so that tuple types match
	let mut values = values.iter().map(|x| x.to_llvm_value(stmts, fun_name).0).collect::<Vec <_>>();
	unsafe { LLVMConstStructInContext(llvm_context(), values.as_mut_ptr(), values.len() as _, 0) }
}

fn build_variable(location: &ExprKindVariableLocation, stmts: &[Stmt], is_lvalue: bool) -> LLVMValueRef {
	match location {
		ExprKindVariableLocation::FunArg {
			fun_stmt_index,
			fun_overload,
			var_index
		} => {
			let fun = match &stmts[*fun_stmt_index] {
				Stmt::FunDef(fun) => fun,
				_ => unreachable!()
			};
			let overload = &fun.overloads[*fun_overload];
			if is_lvalue {
				panic!("fun args cannot be mutable; expected an lvalue")
			}
			unsafe { LLVMGetParam(overload.llvm_fun.unwrap(), *var_index as _) }
		},
		ExprKindVariableLocation::Val {
			fun_stmt_index,
			fun_overload,
			line_def
		} => {
			let fun = match &stmts[*fun_stmt_index] {
				Stmt::FunDef(fun) => fun,
				_ => unreachable!()
			};
			let overload = &fun.overloads[*fun_overload];
			let val = overload.vals.get(line_def).unwrap();
			let llvm_value = val.llvm_value.unwrap();

			if val.mutable && !is_lvalue {
				unsafe { LLVMBuildLoad(llvm_builder(), llvm_value, b"\0".as_ptr() as _) }
			} else if !val.mutable && is_lvalue {
				panic!("expected an lvalue")
			} else {
				llvm_value
			}
		}
	}
}

fn build_fun_call(fun_stmt_index: usize, fun_overload: usize, args: &Vec <Expr>, stmts: &[Stmt], fun_name: &str) -> LLVMValueRef {
	let fun = match &stmts[fun_stmt_index] {
		Stmt::FunDef(fun) => fun,
		_ => unreachable!()
	};
	let overload = &fun.overloads[fun_overload];
	let mut args = args.iter().map(|x| x.to_llvm_value(stmts, fun_name).0).collect::<Vec <_>>();
	unsafe { LLVMBuildCall(llvm_builder(), overload.llvm_fun.unwrap(), args.as_mut_ptr(), args.len() as _, b"\0".as_ptr() as _) }
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
				let phi = LLVMBuildPhi(llvm_builder(), ty.llvm_type(), b"\0".as_ptr() as _);
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

	fn _to_llvm(&self, stmts: &[Stmt], is_lvalue: bool, fun_name: &str) -> (LLVMValueRef, /* terminated */ bool) {
		(match &self.kind {
			ExprKind::Variable(location) => build_variable(location, stmts, is_lvalue),

			_ if is_lvalue => panic!("expected an lvalue"),

			ExprKind::Tuple(values) => build_tuple(values, stmts, fun_name),
			ExprKind::FunCall { fun_stmt_index, fun_overload, args }
				=> build_fun_call(*fun_stmt_index, *fun_overload, args, stmts, fun_name),
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
