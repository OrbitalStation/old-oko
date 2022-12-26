use crate::*;
use std::collections::HashSet;

pub fn check_each_function_is_unique(stmts: &Vec <Stmt>) {
    let mut funs = HashSet::new();

    for i in stmts {
        if let Stmt::FunDef(def) = i {
            assert!(funs.insert(def.name.as_str()), "function `{}` duplicates", def.name)
        }
    }
}
