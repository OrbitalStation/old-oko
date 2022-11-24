#![feature(type_alias_impl_trait)]

#[macro_export]
macro_rules! modules {
    ($( $name:ident )*) => {$(
		mod $name;
		pub use $name::*;
	)*};
}

modules!(grammar ty stmt tools fun expr parse_fun_input);
