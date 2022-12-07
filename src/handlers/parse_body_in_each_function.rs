use crate::*;

pub fn parse_body_in_each_function(stmts: &mut Vec <Stmt>) {
    let mut line = 0;
    for input in ParseFunBodyInputStruct::new(stmts, &mut line) {
        let mut is_extern_fun = false;

        let bodies = match input.cur() {
            Stmt::FunDef(fun) => {
                let mut bodies = Vec::with_capacity(fun.overloads.len());
                for (overload_idx, overload) in fun.overloads.iter().enumerate() {
                    let code = match &overload.body {
                        FunBody::Raw { code } => code,
                        _ => unreachable!()
                    };

                    let body = if overload.is_simple {
                        let (fun_stmt, _) = handle_complex_body_line(&format!("return {code}\n"), &input.with(overload_idx));
                        fun_stmt
                    } else {
                        let (fun_stmts, ty) = handle_complex_body_line(&*code, &input.with(overload_idx));
                        assert_eq!(ty, Type::UNIT_TUPLE, "a statement in a complex function is not of `()` type");
                        fun_stmts
                    };

                    bodies.push(body)
                }

                bodies
            },
            Stmt::ExternFun(_) => {
                is_extern_fun = true;
                vec![]
            },
            _ => continue
        };

        if is_extern_fun {
            match input.cur_mut() {
                Stmt::ExternFun(fun) => {
                    let args = fun.args.iter().map(|x| x.llvm_type()).collect();
                    fun.llvm_fun = Some(create_llvm_fun(&fun.name, args, &fun.ret_ty))
                },
                _ => unreachable!()
            }
            continue
        }

        let overloads_num = bodies.len();

        match input.cur_mut() {
            Stmt::FunDef(fun) => for (overload_idx, body) in bodies.into_iter().enumerate() {
                if fun.overloads[overload_idx].is_simple {
                    let ty = match &body[0] {
                        FunStmt::Return(expr) => &expr.ty,
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

                let args = fun.overloads[overload_idx].args.iter().map(|x| x.llvm_type()).collect();
                let ret_ty = fun.overloads[overload_idx].ret_ty.as_determined();

                fun.overloads[overload_idx].llvm_fun = Some(create_llvm_fun(&overload_name, args, ret_ty));
                fun.overloads[overload_idx].body = FunBody::Baked(body);
            },
            _ => unreachable!()
        }
    }
}
