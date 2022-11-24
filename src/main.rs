use oko::*;

extern crate llvm_sys as llvm;

fn main() {
    let code = remove_comments(std::fs::read_to_string("code").unwrap().replace("    ", "\t"));

    let mut stmts = parse_raw_oko_code(&code).unwrap();

    check_each_function_is_unique_and_collect_overloads_into_one(&mut stmts);

    parse_body_in_each_function(&mut stmts);

    create_llvm_context();
    create_llvm_module("oko");

    bake_types();

    transpile_statements_into_llvm(stmts);

    unsafe { LLVMDumpModule(current_llvm_module()) }

    drop_llvm_module();
    drop_llvm_context();
}

use llvm::core::*;
use core::ptr::null_mut;
use std::ffi::CString;

unsafe fn codegen(input: &str) {
    let context = LLVMContextCreate();
    let module = LLVMModuleCreateWithName(b"example_module\0".as_ptr() as *const _);
    let builder = LLVMCreateBuilderInContext(context);

    // In LLVM, you get your types from functions.
    let int_type = LLVMInt64TypeInContext(context);
    let function_type = LLVMFunctionType(int_type, null_mut(), 0, 0);
    let function = LLVMAddFunction(module, b"main\0".as_ptr() as *const _, function_type);

    let entry_name = CString::new("entry").unwrap();
    let bb = LLVMAppendBasicBlockInContext(context, function, entry_name.as_ptr());
    LLVMPositionBuilderAtEnd(builder, bb);

    // The juicy part: construct a `LLVMValue` from a Rust value:
    let int_value: u64 = input.parse().unwrap();
    let int_value = LLVMConstInt(int_type, int_value, 0);

    LLVMBuildRet(builder, int_value);

    // Instead of dumping to stdout, let's write out the IR to `out.ll`
    let out_file = CString::new("out.ll").unwrap();
    LLVMPrintModuleToFile(module, out_file.as_ptr(), null_mut());

    LLVMDisposeBuilder(builder);
    LLVMDisposeModule(module);
    LLVMContextDispose(context);
}
