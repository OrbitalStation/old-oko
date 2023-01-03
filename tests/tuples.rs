mod common;

const SRC: &str = r#"
sumdiff a b: u8 = a + b, a - b

simd a b: i32 = a + b, a - b, a * b, a / b

simdExplicit a b: i32 -> (i32, i32, i32, i32) = simd a b

"#;

const RESULT: &str = r#"
; ModuleID = 'oko'
source_filename = "oko"

%.tuple.u8-u8 = type { i8, i8 }
%.tuple.i32-i32-i32-i32 = type { i32, i32, i32, i32 }

define void @sumdiff(i8 %0, i8 %1, %.tuple.u8-u8* %2) {
entry:
  %3 = alloca %.tuple.u8-u8
  %4 = getelementptr inbounds %.tuple.u8-u8, %.tuple.u8-u8* %3, i32 0, i32 0
  %5 = add i8 %0, %1
  store i8 %5, i8* %4
  %6 = getelementptr inbounds %.tuple.u8-u8, %.tuple.u8-u8* %3, i32 0, i32 1
  %7 = sub i8 %0, %1
  store i8 %7, i8* %6
  %8 = load %.tuple.u8-u8, %.tuple.u8-u8* %3
  store %.tuple.u8-u8 %8, %.tuple.u8-u8* %2
  ret void
}

define void @simd(i32 %0, i32 %1, %.tuple.i32-i32-i32-i32* %2) {
entry:
  %3 = alloca %.tuple.i32-i32-i32-i32
  %4 = getelementptr inbounds %.tuple.i32-i32-i32-i32, %.tuple.i32-i32-i32-i32* %3, i32 0, i32 0
  %5 = add i32 %0, %1
  store i32 %5, i32* %4
  %6 = getelementptr inbounds %.tuple.i32-i32-i32-i32, %.tuple.i32-i32-i32-i32* %3, i32 0, i32 1
  %7 = sub i32 %0, %1
  store i32 %7, i32* %6
  %8 = getelementptr inbounds %.tuple.i32-i32-i32-i32, %.tuple.i32-i32-i32-i32* %3, i32 0, i32 2
  %9 = mul i32 %0, %1
  store i32 %9, i32* %8
  %10 = getelementptr inbounds %.tuple.i32-i32-i32-i32, %.tuple.i32-i32-i32-i32* %3, i32 0, i32 3
  %11 = sdiv i32 %0, %1
  store i32 %11, i32* %10
  %12 = load %.tuple.i32-i32-i32-i32, %.tuple.i32-i32-i32-i32* %3
  store %.tuple.i32-i32-i32-i32 %12, %.tuple.i32-i32-i32-i32* %2
  ret void
}

define void @simdExplicit(i32 %0, i32 %1, %.tuple.i32-i32-i32-i32* %2) {
entry:
  %3 = alloca %.tuple.i32-i32-i32-i32
  call void @simd(i32 %0, i32 %1, %.tuple.i32-i32-i32-i32* %3)
  %4 = load %.tuple.i32-i32-i32-i32, %.tuple.i32-i32-i32-i32* %3
  store %.tuple.i32-i32-i32-i32 %4, %.tuple.i32-i32-i32-i32* %2
  ret void
}

"#;

#[test]
fn test() { common::test_single_file(SRC, RESULT) }
