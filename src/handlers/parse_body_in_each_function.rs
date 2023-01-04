use crate::*;

pub fn parse_body_in_each_function(stmts: &mut Vec <Stmt>) {
    let mut idx_loc = vec![0];
    let mut input = ParseFunBodyInputStruct::new(stmts, &mut idx_loc);
    parse_methods_of_type_list(&mut input, Type::type_list(), None);

    for input in ParseFunBodyInputStruct::new(stmts, &mut idx_loc) {
        match input.cur_mut() {
            Stmt::FunDef(fun) => parse_fun_body(fun, &input, None),
            Stmt::ExternFun(fun) => {
                let args = fun.args.iter().map(|x| x.llvm_type(false)).collect();
                fun.llvm_fun = Some(create_llvm_fun(&fun.name, args, &fun.ret_ty));
            },
            _ => continue
        };
    }
}

fn parse_methods_of_type_list(input: &mut ParseFunBodyInputStruct, type_list: &mut TypeList, parent_loc: Option <TypeKindScalarLocation>) {
    let baked = match type_list {
        TypeList::Baked(baked) => baked,
        _ => unreachable!()
    };

    for (ty_idx, ty) in baked.iter_mut().enumerate() {
        if let BakedTypeKind::Ordinary(def) = unsafe { &mut *(&mut ty.kind as *mut BakedTypeKind) } {
            let ty_loc = if let Some(parent_loc) = &parent_loc {
                TypeKindScalarLocation::AssociatedItem {
                    index: ty_idx,
                    mother: Box::new(parent_loc.clone()),
                }
            } else {
                TypeKindScalarLocation::Global { index: ty_idx }
            };
            input.mother_ty = Some(Type::from_kind(TypeKind::Scalar { loc: ty_loc.clone() }));
            for (method_index, method) in def.methods.iter_mut().enumerate() {
                input.fun_loc = FunLocation::Method(FunMethodLocation {
                    ty_loc: ty_loc.clone(),
                    method_index
                });
                parse_fun_body(&mut method.def, &input, Some(method.kind))
            }
            parse_methods_of_type_list(input, &mut def.subtypes, Some(ty_loc))
        }
    }
}

pub fn parse_fun_body(fun: &mut FunDef, input: ParseFunBodyInput, method_info: Option <AssociatedMethodKind>) {
    let code = match &mut fun.body {
        FunBody::Raw { code } => code,
        // Already parsed; ignore
        FunBody::Baked(_) => return
    };

    input.reset_idx_loc();

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
            args.insert(0, kind.modify_llvm_type(ty.llvm_type(false)));
        }
        create_llvm_fun(&format!("{}.{}", ty.name_for_llvm(), fun.name), args, ret_ty)
    } else {
        create_llvm_fun(&fun.name, args, ret_ty)
    });
    fun.body = FunBody::Baked(body);
}
