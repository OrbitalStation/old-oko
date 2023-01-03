mod common;

const SRC: &str = r#"
cmp a b: u16 = a == b

nonCmp a b: u16 = a != b

"#;

const RESULT: &str = r#"
; ModuleID = 'oko'
source_filename = "oko"

define i1 @cmp(i16 %0, i16 %1) {
entry:
  %2 = icmp eq i16 %0, %1
  ret i1 %2
}

define i1 @nonCmp(i16 %0, i16 %1) {
entry:
  %2 = icmp ne i16 %0, %1
  ret i1 %2
}

"#;

#[test]
fn test() { common::test_single_file(SRC, RESULT) }
