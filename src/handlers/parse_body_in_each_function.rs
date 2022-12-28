use crate::*;

pub fn parse_body_in_each_function(stmts: &mut Vec <Stmt>) {
    let mut line = 0;
    let mut input = ParseFunBodyInputStruct::new(stmts, &mut line);
    for (ty_idx, ty) in Type::baked().iter_mut().enumerate() {
        if let BakedTypeKind::Ordinary(def) = unsafe { &mut *(&mut ty.kind as *mut BakedTypeKind) } {
            input.mother_ty = Some(Type::from_kind(TypeKind::Scalar { index: ty_idx }));
            for (method_index, method) in def.methods.iter_mut().enumerate() {
                input.fun_loc = FunLocation::Method(FunMethodLocation {
                    ty_index: TypeDefIndex { index: ty_idx },
                    method_index
                });
                parse_fun_body(&mut method.def, &input, Some(method.kind))
            }
        }
    }

    line = 0;
    for input in ParseFunBodyInputStruct::new(stmts, &mut line) {
        match input.cur_mut() {
            Stmt::FunDef(fun) => parse_fun_body(fun, &input, None),
            Stmt::ExternFun(fun) => {
                let args = fun.args.iter().map(|x| x.llvm_type()).collect();
                fun.llvm_fun = Some(create_llvm_fun(&fun.name, args, &fun.ret_ty));
            },
            _ => continue
        };
    }
}

pub fn parse_fun_body(fun: &mut FunDef, input: ParseFunBodyInput, method_info: Option <AssociatedMethodKind>) {
    let code = match &mut fun.body {
        FunBody::Raw { code } => code,
        // Already parsed; ignore
        FunBody::Baked(_) => return
    };

    let body = if fun.is_simple {
        let (fun_stmt, _) = handle_complex_body_line(&format!("return {code}\n"), input);
        let ty = match &fun_stmt[0] {
            FunStmt::Return(expr) => &expr.ty,
            _ => unreachable!()
        };
        match &mut fun.ret_ty {
            x@FunRetType::Undetermined => *x = FunRetType::Determined(ty.clone()),
            FunRetType::Determined(ret) => if *ret != *ty {
                panic!("return types do not match in function `{}`", fun.name)
            }
        }
        fun_stmt
    } else {
        let (fun_stmts, ty) = handle_complex_body_line(&*code, input);
        assert_eq!(ty, Type::UNIT_TUPLE, "a statement in a complex function is not of `()` type");
        fun_stmts
    };

    let mut args = fun.args.iter().map(|x| x.llvm_type()).collect::<Vec <_>>();
    let ret_ty = fun.ret_ty.as_determined();

    fun.llvm_fun = Some(if let Some(kind) = method_info {
        let ty = input.mother_ty().unwrap();
        if kind != AssociatedMethodKind::Static {
            args.insert(0, kind.modify_llvm_type(ty.llvm_type()));
        }
        create_llvm_fun(&format!("{}.{}", ty.name(), fun.name), args, ret_ty)
    } else {
        create_llvm_fun(&fun.name, args, ret_ty)
    });
    fun.body = FunBody::Baked(body);
}
