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

	rule __ident_continuation() -> char
		= x:(letter() / digit()) { x }

	rule ident() -> String
		= ident:$(letter() __ident_continuation()*) { ident.to_string() }

	rule __ty_mut_ref() -> bool
		= "&" { false }
		/ "$" { true }

	rule __non_ptr_ty_start(existing: bool, mother_ty: Option <&Type>) -> Type
		= "Y" !__ident_continuation() {? Ok(mother_ty.ok_or("type")?.clone()) }
		/ name:ident() {? if existing { Type::get_existing_scalar(name).ok_or("type") } else { Ok(Type::meet_new_raw_scalar(None, name, None)) } }
		/ "(" _ ty:__ty(existing, mother_ty) _ ")" { ty }

	rule __non_ptr_ty_cont1() -> String
		= x:ident() { x }
		/ "(" _ x:__non_ptr_ty_cont1() _ ")" { x }

	rule __non_ptr_ty_cont_helper_propagate(x: Result <TypeKindScalarLocation, Vec <String>>) -> Result <TypeKindScalarLocation, Vec <String>>
		= "" { x }

	rule __non_ptr_ty_cont(loc: Result <TypeKindScalarLocation, Vec <String>>) -> Result <TypeKindScalarLocation, Vec <String>>
		= _ "." _ name:__non_ptr_ty_cont1() x:__non_ptr_ty_cont_helper_propagate(open_option_in_arg!(__non_ptr_ty_cont_helper(loc, name))) next:__non_ptr_ty_cont(x.clone())?
	{
		match next {
			Some(x) => x,
			None => x
		}
	}

	rule __non_ptr_ty(existing: bool, mother_ty: Option <&Type>) -> Type
		= start:__non_ptr_ty_start(existing, mother_ty) cont:__non_ptr_ty_cont(__non_ptr_ty_helper(&start))?
		{
			match cont {
				Some(x) => match x {
					Ok(loc) => Type::from_kind(TypeKind::Scalar { loc }),
					Err(continuation) => Type::seq(start, continuation)
				},
				None => start
			}
		}
		/ "[" _ ty:__ty(existing, mother_ty) __ "x" __ num:$(digit()+) _ "]" { Type::array(ty, num) }
		/ "(" _ types:(__ty(existing, mother_ty) ** (_ "," _)) _ ("," _)? ")" { Type::tuple(types) }
		/ mutable:__ty_mut_ref() _ ty:__ty(existing, mother_ty) { Type::reference(ty, mutable) }

	rule __ty(existing: bool, mother_ty: Option <&Type>) -> Type
		= ptrs:$(['*' | '^']+) ty:__non_ptr_ty(existing, mother_ty) { Type::pointer(ty, ptrs) }
		/ ty:__non_ptr_ty(existing, mother_ty) { ty }

	rule ty(mother_ty: Option <&Type>) -> Type
		= x:__ty(false, mother_ty) { x }

	rule existing_ty(mother_ty: Option <&Type>) -> Type
		= x:__ty(true, mother_ty) { x }

	rule typedef_assoc_items_raw() -> String
		= line:complex_body_line_with_nl()* { line.join("\n") }

	rule typedef_assoc_item(mother_ty: &Type) -> AssociatedItem
		= fun:fun_or_extern_fun_definition(Some(mother_ty))
	{
		AssociatedItem::Method(AssociatedMethod {
			def: match fun.0 {
				Stmt::FunDef(def) => def,
				_ => unreachable!()
			},
			kind: fun.1.unwrap(),
			state_of_i: VariableState::valid(mother_ty)
		})
	}
		/ type_definition(Some(mother_ty.as_scalar_loc().unwrap()))
	{ AssociatedItem::Type }

	pub(in crate) rule typedef_assoc_items(mother_ty: &Type) -> Vec <AssociatedItem>
		= items:typedef_assoc_item(mother_ty)* nl()? { items }

	rule __type_definition_add(mother: Option <&TypeKindScalarLocation>) -> Type = name:ident() {
		Type::meet_new_raw_scalar(mother, name.clone(), Some(TypeDef {
			name,
			// Will be replaced
			variants: vec![],
			methods: vec![],
			subtypes: TypeList::Raw(vec![])
		}))
	}

	rule __type_alias_definition_add(mother: Option <&TypeKindScalarLocation>) -> TypeKindScalarLocation = name:ident() {
		Type::add_new_raw_alias(mother, name)
	}

	rule __type_alias_definition_body(prev_mother: Option <&TypeKindScalarLocation>, mother: &TypeKindScalarLocation)
		= _ "=" _ "alias" __ ty:ty(Some(&mother.simple_scalar())) nl()
	{
		match mother.raw().unwrap() {
			RawType::Alias { to, .. } => *to = ty,
			_ => unreachable!()
		}
	}
		/ ""
	{?
		// Cleanup unsuccessful try
		match prev_mother.map(|mother| &mut mother.type_def().unwrap().subtypes).unwrap_or(Type::type_list()) {
			TypeList::Raw(raw) => raw,
			_ => unimplemented!()
		}.pop();
		Err("type alias")
	}

	rule __type_alias_definition(mother: Option <&TypeKindScalarLocation>) -> TypeKindScalarLocation
		= "ty" __ loc:__type_alias_definition_add(mother) __type_alias_definition_body(mother, &loc)
	{ loc }

	rule type_definition(mother: Option <&TypeKindScalarLocation>) -> TypeKindScalarLocation
		= x:__type_alias_definition(mother) { x }
		/ "ty" __ ty:__type_definition_add(mother) variants:type_definition_body(&ty) assoc_items:typedef_assoc_items_raw()
	{
		let loc = ty.as_scalar_loc().unwrap();

		let items = typedef_assoc_items(&(assoc_items + "\n"), &ty).unwrap();

		let typedef = loc.type_def().unwrap();
		typedef.methods = items.into_iter().filter_map(|x| match x {
			AssociatedItem::Method(x) => Some(x),
			_ => None
		}).collect();
		typedef.variants = variants;

		loc.clone()
	}

	rule type_definition_body(mother_ty: &Type) -> Vec <EnumVariant> = kind:(
		typedef_inline_enum(mother_ty)
		/ typedef_wide_enum(mother_ty)
		/ typedef_inline_struct(mother_ty)
		/ typedef_wide_struct(mother_ty)
	) { kind }

	rule typedef_struct_field(mother_ty: &Type) -> TypedefStructFieldRuleReturn
		= names:ident() ++ __ _ ":" _ ty:ty(Some(mother_ty))
	{
		names.into_iter().map(move |name| StructField {
			name,
			ty: ty.clone()
		})
	}

	rule typedef_struct_fields(mother_ty: &Type) -> Vec <StructField> = x:typedef_struct_field(mother_ty) ++ (_ "+" _) {
		x.into_iter().flatten().collect()
	}

	rule typedef_inline_struct(mother_ty: &Type) -> Vec <EnumVariant> = _ "=" _ fields:(typedef_struct_field(mother_ty) ++ (_ "+" _)) nl() {
		TypeDef::from_fields(fields)
	}

	rule typedef_wide_struct_field(mother_ty: &Type) -> TypedefStructFieldRuleReturn
		= "\t" v:typedef_struct_field(mother_ty) nl()
	{ v }

	rule typedef_wide_struct(mother_ty: &Type) -> Vec <EnumVariant> = nl() fields:typedef_wide_struct_field(mother_ty)+ {
		TypeDef::from_fields(fields)
	}

	rule __uppercased_ident() -> String = i:ident() {?
		if i.chars().next().unwrap().is_uppercase() {
			Ok(i)
		} else {
			Err("an uppercase ident")
		}
	}

	rule fail_on_true(should_fail: bool) = quiet!{""} {?
		if should_fail {
			Err("")
		} else {
			Ok(())
		}
	}

	rule __enum_variant_attached_data_fields_expected1 <T> (x: rule <T>, tabs: bool)
		= x() {? if tabs { Ok(()) } else { Err("") } }
		/ "" {? if !tabs { Ok(()) } else { Err("") } }

	rule __enum_variant_attached_data_fields_tabs2(tabs: bool)
		= "\t" __enum_variant_attached_data_fields_expected1(<"\t">, tabs)

	rule __enum_variant_attached_data_field(mother_ty: &Type) -> TypedefStructFieldRuleReturn
		= __enum_variant_attached_data_fields_tabs2(true) fields:typedef_struct_field(mother_ty)
	{ fields }

	rule __enum_variant_attached_data_fields(mother_ty: &Type) -> Vec <StructField>
		= fields:__enum_variant_attached_data_field(mother_ty) ++ nl()
	{ fields.into_iter().flatten().collect() }

	rule __enum_variant_attached_data(mother_ty: &Type, is_inline: bool) -> TypeVariantAttachedData
		= __ fields:typedef_struct_fields(mother_ty) { TypeVariantAttachedData::Struct { fields, llvm_type: None } }
		/ __ tuple:ty(Some(mother_ty)) ++ __ { TypeVariantAttachedData::Tuple(Type::tuple(tuple)) }
		/ fail_on_true(is_inline) nl() fields:__enum_variant_attached_data_fields(mother_ty) { TypeVariantAttachedData::Struct { fields, llvm_type: None } }
		/ "" { TypeVariantAttachedData::None }

	rule __typedef_enum_variant(mother_ty: &Type, is_inline: bool) -> EnumVariant
		= __enum_variant_attached_data_fields_expected1(<"\t">, !is_inline) name:__uppercased_ident() data:__enum_variant_attached_data(mother_ty, is_inline)
	{ EnumVariant { name, data } }

	rule typedef_inline_enum(mother_ty: &Type) -> Vec <EnumVariant> = _ "=" _ variants:__typedef_enum_variant(mother_ty, true) ++ (_ "|" _) nl() {
		variants
	}

	rule typedef_wide_enum(mother_ty: &Type) -> Vec <EnumVariant> = nl() variants:__typedef_enum_variant(mother_ty, false) ++ nl() nl()? {
		variants
	}

	pub(in crate) rule __expr1_variable(input: ParseFunBodyInput) -> Expr = name:ident() {?
		Ok(if let Some((var_index, arg)) = input.cur_fun().args.iter().enumerate().find(|(_, x)| x.name == name) {
			Expr {
				kind: ExprKind::Variable {
					location: ExprKindVariableLocation::FunArg {
						fun: input.fun_loc.clone(),
						var_index
					}
				},
				ty: arg.ty.clone()
			}
		} else if let Some((line, info)) = input.cur_fun().vals.iter().find(|(line, x)| **line < input.line() && x.name == name) {
			Expr {
				kind: ExprKind::Variable {
					location: ExprKindVariableLocation::Val {
						fun: input.fun_loc.clone(),
						line_def: *line
					}
				},
				ty: info.init.ty.clone()
			}
		} else if name == "i" {
			if let FunLocation::Method(method) = &input.fun_loc {
				let kind = FunLocation::Method(method.clone()).method().kind;
				if kind != AssociatedMethodKind::Static {
					Expr {
						kind: ExprKind::Variable {
							location: ExprKindVariableLocation::IInMethod {
								method: method.clone()
							}
						},
						ty: kind.modify_type(Type {
							kind: TypeKind::Scalar {
								loc: method.ty_loc.clone()
							}
						})
					}
				} else {
					return Err("expression")
				}
			} else {
				return Err("expression")
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

	rule __expr1_associated_item_prefix(input: ParseFunBodyInput) -> Result <Type, Expr>
		= ty:existing_ty(input.mother_ty()) _ "." _ { Ok(ty) }
		/ "(" _ x:expr(input) _ ")" _ "." _ { Err(x) }

	rule __expr1_fun_call_helper <'a> (input: ParseFunBodyInput <'a>) -> (FunLocation, &'a mut FunDef, usize, Option <Expr>)
		= ty:__expr1_associated_item_prefix(input)? names:ident() ++ (_ "." _)
	{? get_fun(ty, input, names).ok_or("function call") }

	rule __expr1_fun_call(input: ParseFunBodyInput) -> Expr
		= i:__expr1_fun_call_helper(input) args:__expr1_fun_call_argument(input, i.2)?
	{?
		let (fun_loc, fun_def, _, i_of_method) = i;

		let mut args = args.unwrap_or(vec![]);

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

		Ok(Expr {
			kind: ExprKind::FunCall {
				fun: fun_loc.clone(),
				args
			},
			ty: fun_def.ret_ty_as_determined(input, method_info, fun_loc).clone()
		})
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
		assert_eq!(cond.ty, Type::get_by_name("bool"), "`if` condition must be of `bool` type");
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
		= prefix:__expr1_associated_item_prefix(input)? fields:ident() ++ (_ "." _) e:__expr1_access_inner(open_option_in_arg!(access_field_inner(prefix, input, fields)))
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
		x:(@) __expr1_bin_op(<"*">, input) y:@ { check2arithmetic(x, y, BinOpType::Mul) }
		x:(@) __expr1_bin_op(<"/">, input) y:@ { check2arithmetic(x, y, BinOpType::Div) }
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
				ty: Type::tuple(tuple.iter().map(|x| x.ty.clone()).collect()),
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
		match &mut fun.ret_ty {
			FunRetType::Determined(ret) => if expr.ty != *ret {
				if !expr.ty.try_implicitly_convert(ret, Some(&mut expr.kind)) {
					panic!("return type mismatch in function `{}` with types `{:?}` and `{:?}`", fun.name, ret, expr.ty)
				}
			},
			ret@FunRetType::Undetermined => {
				if matches!(expr.ty.kind, TypeKind::Integer) {
					expr.ty.try_implicitly_convert(&Type::get_by_name("i32"), Some(&mut expr.kind));
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
		input.cur_fun_mut().vals.insert(input.line(), VariableInfo {
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
		/ x:__fun_stmt_assign_short_assign(input, <"/">, BinOpType::Div) { x }
		/ x:__fun_stmt_assign_short_assign(input, <"*">, BinOpType::Mul) { x }

	rule fun_stmt(input: ParseFunBodyInput) -> (FunStmt, Type)
		= ret:__fun_stmt_return(input) { ret }
		/ val:__fun_stmt_val_def(input) { val }
		/ assign:__fun_stmt_assign(input) { assign }
		/ expr:expr(input)
	{
		let ty = expr.ty.clone();
		(FunStmt::Expr(expr), ty)
	}

	rule fundef_arg(mother_ty: Option <&Type>) -> FunDefArgRuleReturn
		= names:ident() ++ __ _ ":" _ ty:ty(mother_ty)
	{
		names.into_iter().map(move |name| FunArg {
			name,
			ty: ty.clone(),
			state: VariableState::Scalar(VariableStateScalar::Valid)
		})
	}

	rule fundef_args(mother_ty: Option <&Type>) -> Vec <FunArg>
		= args:fundef_arg(mother_ty) ** (_ "," _)
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

	rule __fun_ret_ty(mother_ty: Option <&Type>) -> Type
		= _ "->" _ ty:ty(mother_ty) { ty }

	rule __fun_or_extern_fun_definition_cont() -> Option <(bool, String)>
		= _ "=" _ "extern" nl() { None }
		/ body:fundef_body() { Some(body) }

	rule __fun_method_type() -> AssociatedMethodKind
		= _ "." x:$("$!" / "&" / "$" / "!" / "*")
	{
		match x {
			"&" => AssociatedMethodKind::ByRef,
			"$" => AssociatedMethodKind::ByMutRef,
			"!" => AssociatedMethodKind::ByValue,
			"$!" => AssociatedMethodKind::ByMutValue,
			"*" => AssociatedMethodKind::Static,
			_ => unreachable!()
		}
	}

	rule fun_or_extern_fun_definition(mother_ty: Option <&Type>) -> (Stmt, Option <AssociatedMethodKind>)
		= name:ident() method_type:__fun_method_type()? _ args:fundef_args(mother_ty) ret_ty:__fun_ret_ty(mother_ty)? body:__fun_or_extern_fun_definition_cont()
	{
		let is_a_method = mother_ty.is_some();

		match body {
			None => {
				assert!(method_type.is_none() && !is_a_method, "cannot have an extern fun `{name}` as an associated method");
				(Stmt::ExternFun(ExternFun {
					name,
					args: args.into_iter().map(|arg| arg.ty).collect(),
					ret_ty: match ret_ty {
						None => Type::UNIT_TUPLE,
						Some(ty) => ty
					},
					llvm_fun: None
				}), None)
			},
			Some((is_simple, code)) => {
				let assoc_kind = if is_a_method {
					Some(method_type.expect("method kind not specified"))
				} else {
					assert!(method_type.is_none(), "an associated method `{name}` is defined in the global space");
					None
				};
				(Stmt::FunDef(FunDef {
					name,
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
				}), assoc_kind)
			}
		}
	}

	rule stmt() -> Stmt
		= x:type_definition(None) { Stmt::TypeDef(x) }
		/ x:fun_or_extern_fun_definition(None) { x.0 }

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
