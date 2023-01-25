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

enum Outer = empty
	enum Inner = empty
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
%Outer = type {}
%Outer.Inner = type {}
%Outer.Inner.VeryInner = type { i32 }
%Wrapper2 = type { %.tuple. }
%.tuple. = type {}

define %Hash* @Hash.id(%Hash* %0) {
entry:
  ret %Hash* %0
}

define void @Hash.new(i32 %0, i32 %1, %Hash* %2) {
entry:
  %3 = getelementptr inbounds %Hash, %Hash* %2, i32 0, i32 0
  store i32 %0, i32* %3
  %4 = getelementptr inbounds %Hash, %Hash* %2, i32 0, i32 1
  store i32 %1, i32* %4
  ret void
}

define i32 @Wrapper.unwrap(%Wrapper* %0) {
entry:
  %1 = getelementptr inbounds %Wrapper, %Wrapper* %0, i32 0, i32 0
  %2 = load i32, i32* %1
  ret i32 %2
}

define void @Wrapper.new(i32 %0, %Wrapper* %1) {
entry:
  %2 = getelementptr inbounds %Wrapper, %Wrapper* %1, i32 0, i32 0
  store i32 %0, i32* %2
  ret void
}

define void @Outer.empty(%Outer* %0) {
entry:
  ret void
}

define void @Outer.Inner.empty(%Outer.Inner* %0) {
entry:
  ret void
}

define i32 @Outer.Inner.VeryInner.twentySeven() {
entry:
  ret i32 27
}

define void @Outer.Inner.VeryInner.new(i32 %0, %Outer.Inner.VeryInner* %1) {
entry:
  %2 = getelementptr inbounds %Outer.Inner.VeryInner, %Outer.Inner.VeryInner* %1, i32 0, i32 0
  store i32 %0, i32* %2
  ret void
}

define %Wrapper2* @Wrapper2.id(%Wrapper2* %0) {
entry:
  ret %Wrapper2* %0
}

define void @Wrapper2.new(%.tuple. %0, %Wrapper2* %1) {
entry:
  %2 = getelementptr inbounds %Wrapper2, %Wrapper2* %1, i32 0, i32 0
  store %.tuple. %0, %.tuple.* %2
  ret void
}

define i32 @twentySeven() {
entry:
  %0 = call i32 @Outer.Inner.VeryInner.twentySeven()
  ret i32 %0
}


"#;

#[test]
fn test() { common::test_single_file(SRC, RESULT) }
