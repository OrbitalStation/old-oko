mod common;

const SRC: &str = r#"
add a b: i32 -> i32
	a_ := a
	b_ := b
	c := a_ + b_
	c_ := c
	return c_

inc x: u8 -> u8
	$x_ := x
	one := identity 1
	x_ += one
	return x_

identity x: u8 = x

"#;

const RESULT: &str = r#"
; ModuleID = 'oko'
source_filename = "oko"

define i32 @add(i32 %0, i32 %1) {
entry:
  %2 = add i32 %0, %1
  ret i32 %2
}

define i8 @identity(i8 %0) {
entry:
  ret i8 %0
}

define i8 @inc(i8 %0) {
entry:
  %1 = alloca i8
  store i8 %0, i8* %1
  %2 = call i8 @identity(i8 1)
  %3 = load i8, i8* %1
  %4 = add i8 %3, %2
  store i8 %4, i8* %1
  %5 = load i8, i8* %1
  ret i8 %5
}

"#;

#[test]
fn test() { common::test_single_file(SRC, RESULT) }
