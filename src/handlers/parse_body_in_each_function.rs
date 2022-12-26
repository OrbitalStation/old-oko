use crate::*;

pub fn parse_body_in_each_function(stmts: &mut Vec <Stmt>) {
    let mut line = 0;
    for input in ParseFunBodyInputStruct::new(stmts, &mut line) {
        let mut is_extern_fun = false;

        let body = match input.cur() {
            Stmt::FunDef(fun) => {
                let code = match &fun.body {
                    FunBody::Raw { code } => code,
                    _ => unreachable!()
                };

                if fun.is_simple {
                    let (fun_stmt, _) = handle_complex_body_line(&format!("return {code}\n"), &input);
                    fun_stmt
                } else {
                    let (fun_stmts, ty) = handle_complex_body_line(&*code, &input);
                    assert_eq!(ty, Type::UNIT_TUPLE, "a statement in a complex function is not of `()` type");
                    fun_stmts
                }
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

        match input.cur_mut() {
            Stmt::FunDef(fun) => {
                if fun.is_simple {
                    let ty = match &body[0] {
                        FunStmt::Return(expr) => &expr.ty,
                        _ => unreachable!()
                    };

                    match &mut fun.ret_ty {
                        x@FunRetType::Undetermined => *x = FunRetType::Determined(ty.clone()),
                        FunRetType::Determined(ret) => if *ret != *ty {
                            panic!("return types do not match in function `{}`", fun.name)
                        }
                    }
                }

                let args = fun.args.iter().map(|x| x.llvm_type()).collect();
                let ret_ty = fun.ret_ty.as_determined();

                fun.llvm_fun = Some(create_llvm_fun(&fun.name, args, ret_ty));
                fun.body = FunBody::Baked(body);
            },
            _ => unreachable!()
        }
    }
}
