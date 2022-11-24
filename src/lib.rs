#![feature(type_alias_impl_trait)]

pub extern crate llvm_sys as llvm;

#[macro_export]
macro_rules! modules {
    ($( $name:ident )*) => {$(
		mod $name;
		pub use $name::*;
	)*};
}

modules!(grammar ty stmt tools fun expr parse_fun_input extern_fun builtin_ty handlers llvm_tools);
