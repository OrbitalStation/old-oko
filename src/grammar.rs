use peg::RuleResult;
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

	rule ident() -> String
		= ident:$(letter() (letter() / digit())*) { ident.to_string() }

	rule ty() -> Type
		= ptrs:$(['*' | '^']*) name:ident() { Type::meet_new_raw_scalar(ptrs, name, None) }
		/ "(" _ types:(ty() ** (_ "," _)) _ ")" { Type::from_kind(TypeKind::Tuple { types }) }

	rule type_definition() -> TypeDefIndex
		= "ty" __ name:ident() kind:type_definition_body()
	{
		let ty = Type::meet_new_raw_scalar("", name.clone(), Some(TypeDef {
			name,
			kind
		}));

		TypeDefIndex {
			index: ty.as_scalar_index()
		}
	}

	rule type_definition_body() -> TypeDefKind = kind:(
		typedef_opaque()
		/ typedef_inline_enum()
		/ typedef_wide_enum()
		/ typedef_inline_struct()
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

	rule __expr1_variable(input: ParseFunBodyInput) -> Expr = name:ident() {?
		Ok(if let Some((var_index, arg)) = input.cur_fun().overloads[input.fun_overload].args.iter().enumerate().find(|(_, x)| x.name == name) {
			Expr {
				kind: ExprKind::Variable(ExprKindVariableLocation::FunArg {
					fun_stmt_index: input.cur_stmt,
					fun_overload: input.fun_overload,
					var_index
				}),
				ty: arg.ty.clone()
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

	rule __expr1(input: ParseFunBodyInput) -> Expr = precedence! {
		x:__expr1_fun_call(input) { x }
		--
		"(" _ ")" { Expr::UNIT_TUPLE }
		"(" _ x:expr(input) _ ")" { x }
		x:__expr1_variable(input) { x }
	}

	rule expr(input: ParseFunBodyInput) -> Expr
		= tuple:(__expr1(input) ** (_ "," _)) is_there_a_trailing_comma:(_ "," _)?
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
		= _ ![_] { Expr::UNIT_TUPLE }
		/ __ expr:expr(input) { expr }

	rule fun_stmt(input: ParseFunBodyInput) -> (FunStmt, Type)
		= "return" expr:__fun_stmt_get_return_val(input)
	{
		let fun = input.cur_fun_mut();
		match &mut fun.overloads[input.fun_overload].ret_ty {
			FunRetType::Determined(ret) => if expr.ty != *ret {
				panic!("return type mismatch in function `{}`", fun.name)
			},
			ret@FunRetType::Undetermined => *ret = FunRetType::Determined(expr.ty.clone())
		}

		(FunStmt::Return(Box::new(expr)), Type::UNIT_TUPLE)
	}
		/ expr:expr(input) { (FunStmt::Expr(expr.kind), expr.ty) }

	rule fundef_arg() -> FunDefArgRuleReturn
		= names:ident() ++ __ _ ":" _ ty:ty()
	{
		names.into_iter().map(move |name| FunArg {
			name,
			ty: ty.clone()
		})
	}

	rule fundef_args() -> Vec <FunArg>
		= args:fundef_arg() ** (_ "," _)
	{ args.into_iter().flatten().collect() }

	rule __fundef_complex_body_line() -> String
		= "\t" line:$([^ '\n']+) nl() { line.to_string() }

	rule fundef_body() -> (bool, Vec <String>)
		= _ "=" _ rest:$([^ '\n']+) nl() { (true, vec![rest.to_string()]) }
		/ nl() code:__fundef_complex_body_line()*
	{
		if code.is_empty() {
			panic!("a complex function cannot have empty body")
		}
		(false, code)
	}

	rule __fun_ret_ty() -> Type
		= _ "->" _ ty:ty() { ty }

	rule __fun_or_extern_fun_definition_cont() -> Option <(bool, Vec <String>)>
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
				}
			}),
			Some(body) => {
				let (is_simple, lines) = body;

				Stmt::FunDef(FunDef {
					name,
					overloads: vec![FunDefOverloadablePart {
						args,
						body: FunBody::Raw { lines },
						ret_ty: match ret_ty {
							None if is_simple => FunRetType::Undetermined,
							None => FunRetType::Determined(Type::UNIT_TUPLE),
							Some(ty) => FunRetType::Determined(ty)
						},
						is_simple,
						llvm_fun: None
					}]
				})
			}
		}
	}

	rule stmt() -> Stmt
		= x:type_definition() { Stmt::TypeDef(x) }
		/ x:fun_or_extern_fun_definition() { x }

	rule __global_whitespace() = quiet!{[' ' | '\n' | '\t']*}

	/* PUBLIC SECTION */

	pub rule parse_raw_oko_code() -> Vec <Stmt>
		= __global_whitespace() stmts:(stmt() ** __global_whitespace()) __global_whitespace()
	{ stmts }

	pub rule parse_fun_body_line(input: ParseFunBodyInput) -> (FunStmt, Type)
		= expr:fun_stmt(input) { expr }

} }

pub use okolang::*;