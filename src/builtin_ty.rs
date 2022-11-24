macro_rules! define {
    ($vis:vis const $name:ident = [ $( $expr:expr ),* ]) => {
        $vis const $name: [BuiltinType; define!(@count $(($expr))*)] = [$(
            BuiltinType {
				name: $expr
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
	"u8",
	"i32"
]);

pub struct BuiltinType {
	pub name: &'static str
}
