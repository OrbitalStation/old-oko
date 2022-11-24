use crate::*;

pub fn parse_body_in_each_function(stmts: &mut Vec <Stmt>) {
    for input in ParseFunBodyInputStruct::new(stmts) {
        let bodies = match input.cur() {
            Stmt::FunDef(fun) => {
                let mut bodies = Vec::with_capacity(fun.overloads.len());
                for (overload_idx, overload) in fun.overloads.iter().enumerate() {
                    let code = match &overload.body {
                        FunBody::Raw { lines } => lines,
                        _ => unreachable!()
                    };

                    let body = if overload.is_simple {
                        let expr = parse_fun_body_line(&code[0], &input.with_fun_overload(overload_idx)).unwrap();
                        if matches!(&expr.kind, ExprKind::Return(_)) {
                            panic!("`return` operator is not allowed in a simple function `{}`", fun.name)
                        }
                        vec![Expr::ret(expr).kind]
                    } else {
                        code.iter().map(|line| {
                            let Expr { kind, ty } = parse_fun_body_line(&line, &input.with_fun_overload(overload_idx)).unwrap();
                            if ty != Type::UNIT_TUPLE {
                                panic!("an expression from a complex function is not of `()` type")
                            }
                            kind
                        }).collect::<Vec <_>>()
                    };

                    bodies.push(body)
                }

                bodies
            },
            _ => continue
        };

        let overloads_num = bodies.len();

        match input.cur_mut() {
            Stmt::FunDef(fun) => for (overload_idx, body) in bodies.into_iter().enumerate() {
                if fun.overloads[overload_idx].is_simple {
                    let ty = match &body[0] {
                        ExprKind::Return(expr) => &expr.ty,
                        _ => unreachable!()
                    };

                    match &mut fun.overloads[overload_idx].ret_ty {
                        x@FunRetType::Undetermined => *x = FunRetType::Determined(ty.clone()),
                        FunRetType::Determined(ret) => if *ret != *ty {
                            panic!("return types do not match in function `{}`", fun.name)
                        }
                    }
                }

                let overload_name = if overloads_num == 1 {
                    fun.name.clone()
                } else {
                    format!("{}.{overload_idx}", fun.name)
                };

                let args = fun.overloads[overload_idx].args.iter().map(|x| x.ty.llvm_type()).collect();
                let ret_ty = fun.overloads[overload_idx].ret_ty.as_determined().llvm_type();

                fun.overloads[overload_idx].llvm_fun = Some(create_llvm_fun(&overload_name, args, ret_ty));
                fun.overloads[overload_idx].body = FunBody::Baked(body)
            },
            _ => unreachable!()
        }
    }
}
