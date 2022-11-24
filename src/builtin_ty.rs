use llvm::prelude::*;
use llvm::core::*;

macro_rules! define {
    ($vis:vis const $name:ident = [ $( $ty:literal $llvm_create_fn:ident),* ]) => {
        $vis const $name: [BuiltinType; define!(@count $(($ty))*)] = [$(
            BuiltinType {
				name: $ty,
				llvm_create: $llvm_create_fn
			}
        ),*];
    };

    (@count ($( $tt:tt )*)) => {
        1
    };

    (@count ($( $head:tt )*) $( $tail:tt )+) => {
        define!(@count ($($head)*)) + define!(@count $( $tail )*)
    };
}

define!(pub const BUILTIN_TYPES = [
	"u8" LLVMInt8TypeInContext,
	"i32" LLVMInt32TypeInContext
]);

pub struct BuiltinType {
	pub name: &'static str,
	pub llvm_create: unsafe extern "C" fn(LLVMContextRef) -> LLVMTypeRef
}
