use llvm::prelude::*;
use llvm::core::*;

macro_rules! define {
    ($vis:vis const $name:ident = [ $( $ty:literal $llvm_create_fn:ident $kind:ident),* ]) => {
        $vis const $name: [BuiltinType; define!(@count $(($ty))*)] = [$(
            BuiltinType {
				name: $ty,
				llvm_create: $llvm_create_fn,
				kind: BuiltinTypeKind::$kind
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
	"bool" LLVMInt1TypeInContext Boolean,

	"u8" LLVMInt8TypeInContext Unsigned,
	"u16" LLVMInt16TypeInContext Unsigned,
	"u32" LLVMInt32TypeInContext Unsigned,
	"u64" LLVMInt64TypeInContext Unsigned,

	"i8" LLVMInt8TypeInContext Signed,
	"i16" LLVMInt16TypeInContext Signed,
	"i32" LLVMInt32TypeInContext Signed,
	"i64" LLVMInt64TypeInContext Signed
]);

pub struct BuiltinType {
	pub name: &'static str,
	pub llvm_create: unsafe extern "C" fn(LLVMContextRef) -> LLVMTypeRef,
	pub kind: BuiltinTypeKind
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BuiltinTypeKind {
	Signed,
	Unsigned,
	Boolean
}
