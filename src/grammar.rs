use peg::RuleResult;
use std::collections::HashMap;
use crate::*;

type TypedefStructFieldRuleReturn = impl Iterator <Item = StructField>;

type FunDefArgRuleReturn = impl Iterator <Item = FunArg> + Clone;

macro_rules! open_option_in_arg {
	($option:expr) => {
		match $option {
			Some(x) => x,
			None => return RuleResult::Failed
		}
	}
}

fn _check2arithmetic(mut x: Expr, mut y: Expr, op: BinOpType, ty: impl for <'a> FnOnce(&'a Expr) -> Type) -> Expr {
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

fn check2arithmetic(x: Expr, y: Expr, op: BinOpType) -> Expr {
	assert!(x.ty.is_arithmetic(), "cannot apply `{:?}` to non-arithmetic types", op);
	_check2arithmetic(x, y, op, |x| x.ty.clone())
}

fn check2arithmetic_bool(x: Expr, y: Expr, op: BinOpType) -> Expr {
	_check2arithmetic(x, y, op, |_| Type::get_builtin("bool"))
}

fn check2full_bool(x: Expr, y: Expr, op: BinOpType) -> Expr {
	let bool = Type::get_builtin("bool");
	assert_eq!(x.ty, bool, "cannot apply `{:?}` to non-boolean type", op);
	assert_eq!(y.ty, bool, "cannot apply `{:?}` to non-boolean type", op);
	_check2arithmetic(x, y, op, |_| bool)
}

fn access_field_inner(input: ParseFunBodyInput, fields: Vec <String>) -> Expr {
	let mut cur: Expr = __expr1_variable(&fields[0], input).expect("unknown variable");
	for next in fields.iter().skip(1) {
		let fields = cur.ty.get_fields_of_struct().expect("expected a structure");
		let (idx, field) = fields.iter().enumerate().find(|(_, x)| x.name == *next).expect("no field with such a name found");
		cur = Expr {
			kind: ExprKind::Variable {
				location: ExprKindVariableLocation::AccessField {
					i: Box::new(cur),
					def: fields,
					field: idx
				}
			},
			ty: field.ty.clone(),
		}
	}
	cur
}

fn assignment(input: ParseFunBodyInput, mut lvalue: Expr, mut new: Expr) -> (FunStmt, Type) {
	// if !lvalue.ty.is_copy() {
	//     // TODO! DROP lvalue and do not forget that drop may occur only on fully valid values
	// }

	assert!(lvalue.is_lvalue(input.stmts()), "cannot assign to a non-lvalue");
	new.mark_as_moved_and_panic_if_already(input);
	assert!(new.ty.eq_implicit(&mut lvalue.ty, Some(&mut new.kind), Some(&mut lvalue.kind)), "cannot assign two different types");
	(FunStmt::Assignment { lvalue, new }, Type::UNIT_TUPLE)
}

fn dereference(ptr: Expr) -> Expr {
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

peg::parser! { grammar okolang() for str {

	rule __whitespace_single() = quiet!{[' ' | '\t']}

	rule _ = __whitespace_single()*

	rule __ = __whitespace_single()+

	rule nl() = (_ "\n")+

	rule letter() -> char
		= c:[_]
	{?
		if !c.is_alphabetic() && c != '_' {
			return Err("ident")
		}
		Ok(c)
	}

	rule digit() -> char = c:['0'..='9'] { c }

	rule ident() -> String
		= ident:$(letter() (letter() / digit())*) { ident.to_string() }

	rule __ty_mut_ref() -> bool
		= "&" { false }
		/ "$" { true }

	rule __non_ptr_ty() -> Type
		= name:ident() { Type::meet_new_raw_scalar(name, None) }
		/ "[" _ ty:ty() __ "x" __ num:$(digit()+) _ "]" { Type::meet_new_array(ty, num) }
		/ "(" _ types:(ty() ** (_ "," _)) _ ")" { Type::from_kind(TypeKind::Tuple { types }) }
		/ mutable:__ty_mut_ref() _ ty:ty() { Type::meet_new_reference(ty, mutable) }

	rule ty() -> Type
		= ptrs:$(['*' | '^']+) ty:__non_ptr_ty() { Type::meet_new_pointer(ty, ptrs) }
		/ ty:__non_ptr_ty() { ty }

	rule type_definition() -> TypeDefIndex
		= "ty" __ name:ident() kind:type_definition_body()
	{
		let ty = Type::meet_new_raw_scalar(name.clone(), Some(TypeDef {
			name,
			kind,
			methods: HashMap::new()
		}));

		TypeDefIndex {
			index: ty.as_scalar_index()
		}
	}

	rule type_definition_body() -> TypeDefKind = kind:(
		typedef_opaque()
		/ typedef_inline_struct()
		/ typedef_inline_enum()
		/ typedef_wide_enum()
		/ typedef_wide_struct()
	) { kind }

	rule typedef_opaque() -> TypeDefKind
		= _ "=" _ "opaque" nl() { TypeDefKind::Opaque }

	rule typedef_struct_field() -> TypedefStructFieldRuleReturn
		= names:ident() ++ __ _ ":" _ ty:ty()
	{
		names.into_iter().map(move |name| StructField {
			name,
			ty: ty.clone()
		})
	}

	rule typedef_inline_struct() -> TypeDefKind = _ "=" _ fields:(typedef_struct_field() ++ (_ "+" _)) nl() {
		TypeDefKind::Struct {
			fields: fields.into_iter().flatten().collect()
		}
	}

	rule typedef_wide_struct_field() -> TypedefStructFieldRuleReturn
		= "\t" v:typedef_struct_field() nl()
	{ v }

	rule typedef_wide_struct() -> TypeDefKind = nl() fields:typedef_wide_struct_field()+ {
		TypeDefKind::Struct {
			fields: fields.into_iter().flatten().collect()
		}
	}

	rule typedef_enum_variant() -> EnumVariant
		= name:ident() _ data:ty() ** __
	{ EnumVariant { name, data } }

	rule typedef_inline_enum() -> TypeDefKind = _ "=" _ variants:(typedef_enum_variant() ++ (_ "|" _)) nl() {
		TypeDefKind::Enum {
			variants
		}
	}

	rule typedef_wide_enum_variant() -> EnumVariant
		= "\t" v:typedef_enum_variant() nl()
	{ v }

	rule typedef_wide_enum() -> TypeDefKind = nl() variants:typedef_wide_enum_variant()+ {
		TypeDefKind::Enum {
			variants
		}
	}

	pub(in crate) rule __expr1_variable(input: ParseFunBodyInput) -> Expr = name:ident() {?
		Ok(if let Some((var_index, arg)) = input.cur_fun().overloads[input.fun_overload].args.iter().enumerate().find(|(_, x)| x.name == name) {
			Expr {
				kind: ExprKind::Variable {
					location: ExprKindVariableLocation::FunArg {
						fun_stmt_index: input.cur_stmt,
						fun_overload: input.fun_overload,
						var_index
					}
				},
				ty: arg.ty.clone()
			}
		} else if let Some((line, info)) = input.cur_fun().overloads[input.fun_overload].vals.iter().find(|(line, x)| **line < input.line() && x.name == name) {
			Expr {
				kind: ExprKind::Variable {
					location: ExprKindVariableLocation::Val {
						fun_stmt_index: input.cur_stmt,
						fun_overload: input.fun_overload,
						line_def: *line
					}
				},
				ty: info.init.ty.clone()
			}
		} else {
			return Err("expression")
		})
	}

	rule __expr1_fun_call_argument(input: ParseFunBodyInput, remain: usize) -> Vec <Expr>
		= __ expr:expr(input) next:__expr1_fun_call_argument(input, remain.wrapping_sub(1))?
	{?
		if remain == 0 {
			Err("expression")
		} else {
			let mut r = vec![expr];
			r.extend_from_slice(&next.unwrap_or(vec![]));
			Ok(r)
		}
	}

	rule __expr1_fun_call(input: ParseFunBodyInput) -> Expr
		= fun:ident() args:__expr1_fun_call_argument(input, open_option_in_arg!(input.fun_by_name(&fun)).1.overloads[0].args.len())?
	{?
		let (fun_stmt_index, fun) = input.fun_by_name(&fun).ok_or("function call")?;

		for (fun_overload, overload) in fun.overloads.iter().enumerate() {
			let args = match &args {
				Some(x) => x.clone(),
				None if overload.args.is_empty() => vec![],
				None => continue
			};

			if args.len() != overload.args.len() {
				continue
			}

			for (idx, arg) in args.iter().enumerate() {
				if arg.ty != overload.args[idx].ty {
					continue
				}
			}

			for arg in &args {
				arg.mark_as_moved_and_panic_if_already(input)
			}

			return Ok(Expr {
				kind: ExprKind::FunCall {
					fun_stmt_index,
					fun_overload,
					args
				},
				ty: overload.ret_ty.as_determined().clone()
			})
		}

		panic!("no matching overload for function call of `{}` in function `{}`", fun.name, input.cur_fun().name)
	}

	rule __expr1_extern_fun_call(input: ParseFunBodyInput) -> Expr
		= fun:ident() args:__expr1_fun_call_argument(input, open_option_in_arg!(input.extern_fun_by_name(&fun)).1.args.len())?
	{?
		let (fun_stmt_index, fun) = input.extern_fun_by_name(&fun).ok_or("function call")?;
		let args = args.unwrap_or(vec![]);
		assert_eq!(args.len(), fun.args.len(), "wrong number of arguments");
		for (idx, arg) in args.iter().enumerate() {
			arg.mark_as_moved_and_panic_if_already(input);
			assert_eq!(arg.ty, fun.args[idx], "incorrect type of an argument")
		}
		Ok(Expr {
			kind: ExprKind::ExternFunCall {
				fun_stmt_index,
				args
			},
			ty: fun.ret_ty.clone()
		})
	}

	rule __expr1_bin_op <T> (op: rule <T>, input: ParseFunBodyInput)
		= (__ op() __) {}
		/ op() {}

	rule __expr1_literal() -> Expr
		= x:$(digit()+)
	{
		let x = x.parse().unwrap();
		Expr {
			kind: ExprKind::Literal(ExprLiteral::Integer(x)),
			ty: Type::from_kind(TypeKind::Integer)
		}
	}

	rule __expr1_if_body_complex_branch_part(input: ParseFunBodyInput) -> Vec <FunStmt>
		= nl() b:complex_body_line() ** nl()
	{
		assert!(!b.is_empty(), "`if` branch cannot be empty");
		handle_complex_body_line(&(b.join("\n") + "\n"), input).0
	}

	rule __expr1_if_body_complex_else_branch(input: ParseFunBodyInput) -> Vec <FunStmt>
		= nl() "else" no:__expr1_if_body_complex_branch_part(input) { no }
		/ (nl() / __) "else" __ stmt:fun_stmt(input) { vec![stmt.0] }

	rule __expr1_if_body_yes(input: ParseFunBodyInput) -> Vec <FunStmt>
		= yes:__expr1_if_body_complex_branch_part(input) { yes }
		/ __ "do" __ stmt:fun_stmt(input) { vec![stmt.0] }

	rule __expr1_if_body(input: ParseFunBodyInput) -> (Vec <FunStmt>, Vec <FunStmt>, Type)
		= yes:__expr1_if_body_yes(input) no:__expr1_if_body_complex_else_branch(input)?
	{
		let mut yes = yes;

		let yes_last = yes.last_mut().unwrap().as_expr_mut();

		let (no, yes_ty) = if let Some(mut no) = no {
			let no_last = no.last_mut().unwrap().as_expr_mut();
			let (cond, yes_ty) = if let Some(yes) = yes_last {
				if let Some(no) = no_last {
					let cond = yes.ty.eq_implicit(&mut no.ty, Some(&mut yes.kind), Some(&mut no.kind));
					let ty = yes.ty.clone();
					yes.mark_as_moved_and_panic_if_already(input);
					no.mark_as_moved_and_panic_if_already(input);
					(cond, ty)
				} else {
					(yes.ty == Type::UNIT_TUPLE, Type::UNIT_TUPLE)
				}
			} else if let Some(no) = no_last {
				(no.ty == Type::UNIT_TUPLE, Type::UNIT_TUPLE)
			} else {
				(true, Type::UNIT_TUPLE)
			};
			assert!(cond, "there are different types on `if` branches");
			(no, yes_ty)
		} else {
			if let Some(yes) = yes_last {
				assert_eq!(yes.ty, Type::UNIT_TUPLE, "`then` branch of complex `if` with no `else` branch shall return `()` type");
			}
			(vec![], Type::UNIT_TUPLE)
		};

		(yes, no, yes_ty)
	}

	rule __expr1_if(input: ParseFunBodyInput) -> Expr
		= "if" __ cond:expr(input) body:__expr1_if_body(input)
	{
		assert_eq!(cond.ty, Type::get_builtin("bool"), "`if` condition must be of `bool` type");
		let (yes, no, ty) = body;
		Expr {
			kind: ExprKind::If {
				cond: Box::new(cond),
				yes,
				no
			},
			ty
		}
	}

	rule __expr1_access_inner(fields: Expr) -> Expr
		= "" { fields }

	rule __expr1_access(input: ParseFunBodyInput) -> Expr
		= fields:ident() ** <2,> (_ "." _) e:__expr1_access_inner(access_field_inner(input, fields))
	{ e }

	rule __expr1(input: ParseFunBodyInput) -> Expr = precedence! {
		x:(@) __ "and" __ y:@ { check2full_bool(x, y, BinOpType::And) }
		x:(@) __ "or" __ y:@ { check2full_bool(x, y, BinOpType::Or) }
		--
		x:(@) __expr1_bin_op(<"==">, input) y:@ { check2arithmetic_bool(x, y, BinOpType::Eq) }
		x:(@) __expr1_bin_op(<"!=">, input) y:@ { check2arithmetic_bool(x, y, BinOpType::NotEq) }
		--
		x:(@) __expr1_bin_op(<"+">, input) y:@ { check2arithmetic(x, y, BinOpType::Add) }
		x:(@) __expr1_bin_op(<"-">, input) y:@ { check2arithmetic(x, y, BinOpType::Sub) }
		--
		x:(@) __expr1_bin_op(<"×">, input) y:@ { check2arithmetic(x, y, BinOpType::Mul) }
		x:(@) __expr1_bin_op(<"÷">, input) y:@ { check2arithmetic(x, y, BinOpType::Div) }
		--
		"*" x:(@) { dereference(x) }
		--
		x:__expr1_fun_call(input) { x }
		x:__expr1_extern_fun_call(input) { x }
		x:__expr1_access(input) { x }
		--
		"(" _ ")" { Expr::UNIT_TUPLE }
		"(" _ x:expr(input) _ ")" { x }
		x:__expr1_if(input) { x }
		x:__expr1_variable(input) { x }
		x:__expr1_literal() { x }
	}

	rule expr(input: ParseFunBodyInput) -> Expr
		= tuple:(__expr1(input) ++ (_ "," _)) is_there_a_trailing_comma:(_ "," _)?
	{
		let mut tuple = tuple;
		if tuple.len() == 1 && is_there_a_trailing_comma.is_none() {
			core::mem::replace(&mut tuple[0], Expr::UNIT_TUPLE)
		} else {
			Expr {
				ty: Type::from_kind(TypeKind::Tuple { types: tuple.iter().map(|x| x.ty.clone()).collect() }),
				kind: ExprKind::Tuple(tuple)
			}
		}
	}

	rule __fun_stmt_get_return_val(input: ParseFunBodyInput) -> Expr
		= nl() { Expr::UNIT_TUPLE }
		/ __ expr:expr(input) { expr }

	rule __fun_stmt_return(input: ParseFunBodyInput) -> (FunStmt, Type)
		= "return" expr:__fun_stmt_get_return_val(input)
	{
		expr.mark_as_moved_and_panic_if_already(input);
		let fun = input.cur_fun_mut();
		let mut expr = expr;
		match &mut fun.overloads[input.fun_overload].ret_ty {
			FunRetType::Determined(ret) => if expr.ty != *ret {
				if !expr.ty.try_implicitly_convert(ret, Some(&mut expr.kind)) {
					panic!("return type mismatch in function `{}`", fun.name)
				}
			},
			ret@FunRetType::Undetermined => {
				if matches!(expr.ty.kind, TypeKind::Integer) {
					expr.ty.try_implicitly_convert(&Type::get_builtin("i32"), Some(&mut expr.kind));
				}
				*ret = FunRetType::Determined(expr.ty.clone())
			}
		}

		(FunStmt::Return(Box::new(expr)), Type::UNIT_TUPLE)
	}

	rule __fun_stmt_val_def(input: ParseFunBodyInput) -> (FunStmt, Type)
		= mutable:("$")? name:ident() _ ":=" _ init:expr(input)
	{
		init.mark_as_moved_and_panic_if_already(input);
		input.cur_fun_mut().overloads[input.fun_overload].vals.insert(input.line(), VariableInfo {
			name,
			state: VariableState::valid(&init.ty),
			init,
			mutable: mutable.is_some(),
			llvm_value: None
		});
		(FunStmt::ValDef { line: input.line() }, Type::UNIT_TUPLE)
	}

	rule __fun_stmt_assign_short_assign <T> (input: ParseFunBodyInput, op: rule <T>, kind: BinOpType) -> (FunStmt, Type)
		= lvalue:expr(input) _ op() "=" _ new:expr(input) { assignment(input, lvalue.clone(), check2arithmetic(lvalue, new, kind)) }

	rule __fun_stmt_assign(input: ParseFunBodyInput) -> (FunStmt, Type)
		= lvalue:expr(input) _ "=" _ new:expr(input) { assignment(input, lvalue, new) }
		/ x:__fun_stmt_assign_short_assign(input, <"+">, BinOpType::Add) { x }
		/ x:__fun_stmt_assign_short_assign(input, <"-">, BinOpType::Sub) { x }
		/ x:__fun_stmt_assign_short_assign(input, <"÷">, BinOpType::Div) { x }
		/ x:__fun_stmt_assign_short_assign(input, <"×">, BinOpType::Mul) { x }

	rule fun_stmt(input: ParseFunBodyInput) -> (FunStmt, Type)
		= ret:__fun_stmt_return(input) { ret }
		/ val:__fun_stmt_val_def(input) { val }
		/ assign:__fun_stmt_assign(input) { assign }
		/ expr:expr(input)
	{
		let ty = expr.ty.clone();
		(FunStmt::Expr(expr), ty)
	}

	rule fundef_arg() -> FunDefArgRuleReturn
		= names:ident() ++ __ _ ":" _ ty:ty()
	{
		names.into_iter().map(move |name| FunArg {
			name,
			ty: ty.clone(),
			state: VariableState::Scalar(VariableStateScalar::Valid)
		})
	}

	rule fundef_args() -> Vec <FunArg>
		= args:fundef_arg() ** (_ "," _)
	{ args.into_iter().flatten().collect() }

	rule complex_body_line() -> String
		= "\t" line:$([^ '\n']+) { line.to_string() }

	rule complex_body_line_with_nl() -> String
		= line:complex_body_line() nl() { line }

	rule fundef_body() -> (bool, String)
		= _ "=" _ rest:$([^ '\n']+) nl() { (true, rest.to_string()) }
		/ nl() code:complex_body_line_with_nl()*
	{
		if code.is_empty() {
			panic!("a complex function cannot have empty body")
		}
		(false, code.join("\n") + "\n")
	}

	rule __fun_ret_ty() -> Type
		= _ "->" _ ty:ty() { ty }

	rule __fun_or_extern_fun_definition_cont() -> Option <(bool, String)>
		= _ "=" _ "extern" nl() { None }
		/ body:fundef_body() { Some(body) }

	rule fun_or_extern_fun_definition() -> Stmt
		= name:ident() _ args:fundef_args() ret_ty:__fun_ret_ty()? body:__fun_or_extern_fun_definition_cont()
	{
		match body {
			None => Stmt::ExternFun(ExternFun {
				name,
				args: args.into_iter().map(|arg| arg.ty).collect(),
				ret_ty: match ret_ty {
					None => Type::UNIT_TUPLE,
					Some(ty) => ty
				},
				llvm_fun: None
			}),
			Some((is_simple, code)) => {
				Stmt::FunDef(FunDef {
					name,
					overloads: vec![FunSignature {
						args,
						body: FunBody::Raw { code },
						ret_ty: match ret_ty {
							None if is_simple => FunRetType::Undetermined,
							None => FunRetType::Determined(Type::UNIT_TUPLE),
							Some(ty) => FunRetType::Determined(ty)
						},
						is_simple,
						llvm_fun: None,
						vals: HashMap::new()
					}]
				})
			}
		}
	}

	rule stmt() -> Stmt
		= x:type_definition() { Stmt::TypeDef(x) }
		/ x:fun_or_extern_fun_definition() { x }

	rule __global_whitespace() = quiet!{[' ' | '\n' | '\t']*}

	rule __parse_complex_body_nl(input: ParseFunBodyInput)
		= nl() { input.next_line() }

	/* PUBLIC SECTION */

	pub rule parse_raw_oko_code() -> Vec <Stmt>
		= __global_whitespace() stmts:(stmt() ** __global_whitespace()) __global_whitespace()
	{ stmts }

	pub rule parse_complex_body(input: ParseFunBodyInput) -> Vec <(FunStmt, Type)>
		= exprs:fun_stmt(input) ++ __parse_complex_body_nl(input) nl()? { exprs }

} }

pub use okolang::*;