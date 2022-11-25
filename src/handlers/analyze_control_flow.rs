use crate::*;

pub fn analyze_control_flow(stmts: &mut [Stmt]) {
	for stmt in stmts {
		match stmt {
			Stmt::FunDef(fun) => for overload in &mut fun.overloads {
				analyze(&fun.name, overload)
			},
			_ => ()
		}
	}
}

fn analyze(name: &str, fun: &mut FunDefOverloadablePart) {
	let FunDefOverloadablePart { body, ret_ty, .. } = fun;

	let body = match body {
		FunBody::Baked(baked) => baked,
		_ => unreachable!()
	};

	let ret_ty = ret_ty.as_determined();

	let mut terminated = false;

	for i in 0..body.len() {
		match &body[i] {
			ExprKind::Return(_) => {
				if i + 1 < body.len() {
					println!("WARNING: some statements in function `{name}` are unreachable");
					body.drain(i + 1..);
				}
				terminated = true;
				break
			},
			_ => ()
		}
	}

	if !terminated {
		if *ret_ty == Type::UNIT_TUPLE {
			body.push(ExprKind::Return(Box::new(Expr::UNIT_TUPLE)))
		} else {
			panic!("return statement is missing from function `{name}`")
		}
	}
}
