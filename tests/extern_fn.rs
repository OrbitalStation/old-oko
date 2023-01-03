mod common;

const SRC: &str = r#"
ty Opaque = Opaque

puts s: *u8 -> i32 = extern

foo opaque: Opaque -> i8 = extern

sideEffect data: u64 = extern


"#;

const RESULT: &str = r#"
; ModuleID = 'oko'
source_filename = "oko"

%Opaque = type {}

declare i32 @puts(i8*)

declare i8 @foo(%Opaque)

declare void @sideEffect(i64)


"#;

#[test]
fn test() { common::test_single_file(SRC, RESULT) }
