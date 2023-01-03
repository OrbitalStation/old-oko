mod common;

const SRC: &str = r#"
pass = 2
"#;

const RESULT: &str = r#"
; ModuleID = 'oko'
source_filename = "oko"

define i32 @pass() {
entry:
  ret i32 2
}
"#;

#[test]
fn test() {
	common::test_single_file(SRC, RESULT)
}
