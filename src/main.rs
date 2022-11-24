use std::collections::HashMap;
use oko::*;

fn main() {
    let code = remove_comments(std::fs::read_to_string("code").unwrap().replace("    ", "\t"));

    let mut stmts: Vec <Stmt> = parse_raw_oko_code(&code).unwrap();

    check_each_function_is_unique_and_collect_duplicates_into_one(&mut stmts);

    parse_body_in_each_function(&mut stmts);

    println!("{stmts:#?}");
}

fn parse_body_in_each_function(stmts: &mut Vec <Stmt>) {
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

                fun.overloads[overload_idx].body = FunBody::Baked(body)
            },
            _ => unreachable!()
        }
    }
}

fn check_each_function_is_unique_and_collect_duplicates_into_one(stmts: &mut Vec <Stmt>) {
    let mut funs: HashMap <_, usize> = HashMap::new();

    for i in 0..stmts.len() {
        let previous = match &unsafe { &*(&*stmts as *const Vec <Stmt>) }[i] {
            Stmt::FunDef(fun) => match funs.insert(fun.name.clone(), i) {
                Some(x) => x,
                _ => continue
            },
            _ => continue
        };

        let previous = match core::mem::replace(&mut stmts[previous], Stmt::Stub) {
            Stmt::FunDef(x) => x,
            _ => unreachable!()
        };

        match &mut stmts[i] {
            Stmt::FunDef(fun) => {
                assert_eq!(fun.overloads[0].args.len(), previous.overloads[0].args.len(),
                    "overloading is not possible with changing the amount of arguments, as it is in function `{}`", fun.name);

                'overload: for overload in &previous.overloads {
                    let args1 = fun.overloads[0].args.iter().map(|x| &x.ty);
                    let args2 = overload.args.iter().map(|x| &x.ty);
                    for (arg1, arg2) in args1.zip(args2) {
                        if *arg1 != *arg2 {
                            continue 'overload
                        }
                    }
                    panic!("function `{}` duplicates", fun.name)
                }

                fun.overloads.extend(previous.overloads.into_iter())
            },
            _ => unreachable!()
        }
    }

    stmts.retain(|item| !matches!(item, Stmt::Stub))
}
