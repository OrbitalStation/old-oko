crate::modules!(context module builder);

#[macro_export]
macro_rules! llvm_singleton {
    (
		singleton $name:ident : $ty:ty;
		create($( $tt:tt )*) = $expr:expr;
		drop = $drop:ident;
	) => {
		static mut __SINGLETON: Option <$ty> = None;

		c_like_concat::concat! {
			pub fn llvm_ ## $name() -> $ty {
				unsafe { __SINGLETON.unwrap() }
			}

			pub fn create_llvm_ ## $name($( $tt )*) {
				drop_llvm_ ## $name();
				unsafe { __SINGLETON = Some($expr) }
			}

			pub fn drop_llvm_ ## $name() {
				unsafe {
					if let Some(x) = __SINGLETON {
						$drop(x)
					}
				}
			}
		}
	};
}
