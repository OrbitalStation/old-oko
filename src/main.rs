use oko::*;

extern crate llvm_sys as llvm;

fn main() {
    create_llvm_context();
    create_llvm_module("oko");
    create_llvm_builder();

    let code = remove_comments(std::fs::read_to_string("code").unwrap().replace("    ", "\t"));

    let mut stmts = parse_raw_oko_code(&code).unwrap();

    bake_types();

    check_each_function_is_unique_and_collect_overloads_into_one(&mut stmts);

    parse_body_in_each_function(&mut stmts);

    transpile_statements_into_llvm(&mut stmts);

    unsafe { llvm_sys::core::LLVMDumpModule(llvm_module()) }

    drop_llvm_builder();
    drop_llvm_module();
    drop_llvm_context();
}
