mod common;

const SRC: &str = r#"
ty int = alias i32

ty char = alias i8

ty Hash
	k: int
	c: int

	id.& = i

ty Wrapper = x: int
	unwrap.& -> i32 = i.x

ty OIVI = alias Outer.Inner.VeryInner

ty Outer = Empty
	ty Inner = Empty
		ty VeryInner = x: i32
			twentySeven.* = 27

twentySeven = OIVI.twentySeven

ty RefWrapper = alias &Wrapper2

ty Wrapper2 = inner: ()
	id.* i: RefWrapper = i

"#;

const RESULT: &str = r#"
; ModuleID = 'oko'
source_filename = "oko"

%Hash = type { i32, i32 }
%Wrapper = type { i32 }
%Wrapper2 = type { %.tuple. }
%.tuple. = type {}

define %Hash* @Hash.id(%Hash* %0) {
entry:
  ret %Hash* %0
}

define i32 @Wrapper.unwrap(%Wrapper* %0) {
entry:
  %1 = getelementptr inbounds %Wrapper, %Wrapper* %0, i32 0, i32 0
  %2 = load i32, i32* %1
  ret i32 %2
}

define i32 @Outer.Inner.VeryInner.twentySeven() {
entry:
  ret i32 27
}

define %Wrapper2* @Wrapper2.id(%Wrapper2* %0) {
entry:
  ret %Wrapper2* %0
}

define i32 @twentySeven() {
entry:
  %0 = call i32 @Outer.Inner.VeryInner.twentySeven()
  ret i32 %0
}

"#;

#[test]
fn test() { common::test_single_file(SRC, RESULT) }
