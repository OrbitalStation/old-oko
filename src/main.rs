extern crate llvm_sys as llvm;

use oko::*;

fn main() {
    create_llvm_context();
    create_llvm_module("oko");
    create_llvm_builder();

    let code = std::fs::read_to_string("code").unwrap() + "\n";

    let code = code.replace("    ", "\t");

    let code = remove_comments(code);

    Type::initialize_statics();

    let mut stmts = parse_raw_oko_code(&code).unwrap();

    bake_types();

    check_each_function_is_unique(&mut stmts);

    parse_body_in_each_function(&mut stmts);

    transpile_statements_into_llvm(&mut stmts);

    unsafe { llvm::core::LLVMDumpModule(llvm_module()) }

    drop_llvm_builder();
    drop_llvm_module();
    drop_llvm_context();
}
