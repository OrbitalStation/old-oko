mod common;

const SRC: &str = r#"
identity x: i32 = x

drop x y z: u8 = ()
"#;

const RESULT: &str = r#"
; ModuleID = 'oko'
source_filename = "oko"

%.tuple. = type {}

define i32 @identity(i32 %0) {
entry:
  ret i32 %0
}

define void @drop(i8 %0, i8 %1, i8 %2) {
entry:
  %3 = alloca %.tuple.
  ret void
}
"#;

#[test]
fn test() {
	common::test_single_file(SRC, RESULT)
}
