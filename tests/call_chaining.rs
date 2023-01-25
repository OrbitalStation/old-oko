mod common;

const SRC: &str = r#"
doSmth iter second: Iter = iter.skip(4).zip(second).cycle.skip(14).collect.skip 14

ty Iter = x: i8
    zip.! other: Y = i

    cycle.! = i

    skip.! n: u64 = i

    collect.! = i

"#;

const RESULT: &str = r#"
; ModuleID = 'oko'
source_filename = "oko"

%Iter = type { i8 }

define void @Iter.zip(%Iter* %0, %Iter* %1, %Iter* %2) {
entry:
  %3 = load %Iter, %Iter* %0
  store %Iter %3, %Iter* %2
  ret void
}

define void @Iter.cycle(%Iter* %0, %Iter* %1) {
entry:
  %2 = load %Iter, %Iter* %0
  store %Iter %2, %Iter* %1
  ret void
}

define void @Iter.skip(%Iter* %0, i64 %1, %Iter* %2) {
entry:
  %3 = load %Iter, %Iter* %0
  store %Iter %3, %Iter* %2
  ret void
}

define void @Iter.collect(%Iter* %0, %Iter* %1) {
entry:
  %2 = load %Iter, %Iter* %0
  store %Iter %2, %Iter* %1
  ret void
}

define void @Iter.new(i8 %0, %Iter* %1) {
entry:
  %2 = getelementptr inbounds %Iter, %Iter* %1, i32 0, i32 0
  store i8 %0, i8* %2
  ret void
}

define void @doSmth(%Iter* %0, %Iter* %1, %Iter* %2) {
entry:
  %3 = alloca %Iter
  call void @Iter.skip(%Iter* %0, i64 4, %Iter* %3)
  %4 = alloca %Iter
  call void @Iter.zip(%Iter* %3, %Iter* %1, %Iter* %4)
  %5 = alloca %Iter
  call void @Iter.cycle(%Iter* %4, %Iter* %5)
  %6 = alloca %Iter
  call void @Iter.skip(%Iter* %5, i64 14, %Iter* %6)
  %7 = alloca %Iter
  call void @Iter.collect(%Iter* %6, %Iter* %7)
  %8 = alloca %Iter
  call void @Iter.skip(%Iter* %7, i64 14, %Iter* %8)
  %9 = load %Iter, %Iter* %8
  store %Iter %9, %Iter* %2
  ret void
}


"#;

#[test]
fn test() { common::test_single_file(SRC, RESULT) }
