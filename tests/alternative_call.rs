mod common;

const SRC: &str = r#"
add a b: i32 = identity(a) + identity(b)

identity x: i32 = x

test a b: i32 = add(a, b)

"#;

const RESULT: &str = r#"
; ModuleID = 'oko'
source_filename = "oko"

define i32 @identity(i32 %0) {
entry:
  ret i32 %0
}

define i32 @add(i32 %0, i32 %1) {
entry:
  %2 = call i32 @identity(i32 %0)
  %3 = call i32 @identity(i32 %1)
  %4 = add i32 %2, %3
  ret i32 %4
}

define i32 @test(i32 %0, i32 %1) {
entry:
  %2 = call i32 @add(i32 %0, i32 %1)
  ret i32 %2
}


"#;

#[test]
fn test() { common::test_single_file(SRC, RESULT) }
