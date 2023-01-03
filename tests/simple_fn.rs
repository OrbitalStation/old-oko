mod common;

const SRC: &str = r#"
pass = ()
"#;

const RESULT: &str = r#"
; ModuleID = 'oko'
source_filename = "oko"

%.tuple. = type {}

define void @pass() {
entry:
  %0 = alloca %.tuple.
  ret void
}
"#;

#[test]
fn test() {
	common::test_single_file(SRC, RESULT)
}
