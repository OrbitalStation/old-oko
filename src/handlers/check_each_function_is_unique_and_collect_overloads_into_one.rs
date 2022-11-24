use crate::*;
use std::collections::HashMap;

pub fn check_each_function_is_unique_and_collect_overloads_into_one(stmts: &mut Vec <Stmt>) {
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
