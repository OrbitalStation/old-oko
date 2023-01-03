mod common;

const SRC: &str = r#"
add a b: i32 = a + b

addTo a: $i32, b: i32
	*a += b

sub a b: i32 = a - b

subTo a: $i32, b: i32
	*a -= b

mul a b: i32 = a * b

mulTo a: $i32, b: i32
	*a *= b

sdiv a b: i32 = a / b

sdivTo a: $i32, b: i32
	*a /= b

udiv a b: u32 = a / b

udivTo a: $u32, b: u32
	*a /= b

"#;

const RESULT: &str = r#"
; ModuleID = 'oko'
source_filename = "oko"

define i32 @add(i32 %0, i32 %1) {
entry:
  %2 = add i32 %0, %1
  ret i32 %2
}

define void @addTo(i32* %0, i32 %1) {
entry:
  %2 = load i32, i32* %0
  %3 = add i32 %2, %1
  store i32 %3, i32* %0
  ret void
}

define i32 @sub(i32 %0, i32 %1) {
entry:
  %2 = sub i32 %0, %1
  ret i32 %2
}

define void @subTo(i32* %0, i32 %1) {
entry:
  %2 = load i32, i32* %0
  %3 = sub i32 %2, %1
  store i32 %3, i32* %0
  ret void
}

define i32 @mul(i32 %0, i32 %1) {
entry:
  %2 = mul i32 %0, %1
  ret i32 %2
}

define void @mulTo(i32* %0, i32 %1) {
entry:
  %2 = load i32, i32* %0
  %3 = mul i32 %2, %1
  store i32 %3, i32* %0
  ret void
}

define i32 @sdiv(i32 %0, i32 %1) {
entry:
  %2 = sdiv i32 %0, %1
  ret i32 %2
}

define void @sdivTo(i32* %0, i32 %1) {
entry:
  %2 = load i32, i32* %0
  %3 = sdiv i32 %2, %1
  store i32 %3, i32* %0
  ret void
}

define i32 @udiv(i32 %0, i32 %1) {
entry:
  %2 = udiv i32 %0, %1
  ret i32 %2
}

define void @udivTo(i32* %0, i32 %1) {
entry:
  %2 = load i32, i32* %0
  %3 = udiv i32 %2, %1
  store i32 %3, i32* %0
  ret void
}

"#;

#[test]
fn test() { common::test_single_file(SRC, RESULT) }
