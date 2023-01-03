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

define %Info* @Info.id(%Info* %0) {
entry:
  ret %Info* %0
}

define %Short* @Short.id(%Short* %0) {
entry:
  ret %Short* %0
}

"#;

#[test]
fn test() { common::test_single_file(SRC, RESULT) }
