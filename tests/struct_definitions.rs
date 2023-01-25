mod common;

const SRC: &str = r#"
ty Filter
	iter: u8
	pred: u8

	id.& = i

ty Info
	date timestamp: i32
	otherInfo: u8
	filter: Filter

	id.& = i

ty Short = a: i8 + b: u8 + c: Info
	id.& = i

"#;

const RESULT: &str = r#"
; ModuleID = 'oko'
source_filename = "oko"

%Filter = type { i8, i8 }
%Info = type { i32, i32, i8, %Filter }
%Short = type { i8, i8, %Info }

define %Filter* @Filter.id(%Filter* %0) {
entry:
  ret %Filter* %0
}

define void @Filter.new(i8 %0, i8 %1, %Filter* %2) {
entry:
  %3 = getelementptr inbounds %Filter, %Filter* %2, i32 0, i32 0
  store i8 %0, i8* %3
  %4 = getelementptr inbounds %Filter, %Filter* %2, i32 0, i32 1
  store i8 %1, i8* %4
  ret void
}

define %Info* @Info.id(%Info* %0) {
entry:
  ret %Info* %0
}

define void @Info.new(i32 %0, i32 %1, i8 %2, %Filter* %3, %Info* %4) {
entry:
  %5 = getelementptr inbounds %Info, %Info* %4, i32 0, i32 0
  store i32 %0, i32* %5
  %6 = getelementptr inbounds %Info, %Info* %4, i32 0, i32 1
  store i32 %1, i32* %6
  %7 = getelementptr inbounds %Info, %Info* %4, i32 0, i32 2
  store i8 %2, i8* %7
  %8 = getelementptr inbounds %Info, %Info* %4, i32 0, i32 3
  %9 = load %Filter, %Filter* %3
  store %Filter %9, %Filter* %8
  ret void
}

define %Short* @Short.id(%Short* %0) {
entry:
  ret %Short* %0
}

define void @Short.new(i8 %0, i8 %1, %Info* %2, %Short* %3) {
entry:
  %4 = getelementptr inbounds %Short, %Short* %3, i32 0, i32 0
  store i8 %0, i8* %4
  %5 = getelementptr inbounds %Short, %Short* %3, i32 0, i32 1
  store i8 %1, i8* %5
  %6 = getelementptr inbounds %Short, %Short* %3, i32 0, i32 2
  %7 = load %Info, %Info* %2
  store %Info %7, %Info* %6
  ret void
}


"#;

#[test]
fn test() { common::test_single_file(SRC, RESULT) }
