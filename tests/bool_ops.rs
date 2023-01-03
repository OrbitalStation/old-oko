mod common;

const SRC: &str = r#"
and a b: bool = a and b

or a b: bool = a or b

"#;

const RESULT: &str = r#"
; ModuleID = 'oko'
source_filename = "oko"

define i1 @and(i1 %0, i1 %1) {
entry:
  %2 = and i1 %0, %1
  ret i1 %2
}

define i1 @or(i1 %0, i1 %1) {
entry:
  %2 = or i1 %0, %1
  ret i1 %2
}


"#;

#[test]
fn test() { common::test_single_file(SRC, RESULT) }
