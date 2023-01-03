mod common;

const SRC: &str = r#"
pass = ()

ty NonCopyType = someVeryHeavyInfo: u8
	updateCache.$
		pass
		pass
		i.someVeryHeavyInfo = 4
		pass
		pass

ty SomeType
	x: i32
	b: u16
	info: NonCopyType

	unwrapX.& = i.x

	unwrapB.& -> u16 = i.b

	addToB.$ extra: u16
		i.b += extra

	consumeAsInfo.! = i.info

	consumeAsInfoAndUpdateCache.$! -> NonCopyType
		i.info.updateCache
		return i.info

	* Oopsey
	getWrongAnswer.* = 41

"#;

const RESULT: &str = r#"
; ModuleID = 'oko'
source_filename = "oko"

%.tuple. = type {}
%NonCopyType = type { i8 }
%SomeType = type { i32, i16, %NonCopyType }

define void @pass() {
entry:
  %0 = alloca %.tuple.
  ret void
}

define void @NonCopyType.updateCache(%NonCopyType* %0) {
entry:
  call void @pass()
  call void @pass()
  %1 = getelementptr inbounds %NonCopyType, %NonCopyType* %0, i32 0, i32 0
  store i8 4, i8* %1
  call void @pass()
  call void @pass()
  ret void
}

define i32 @SomeType.unwrapX(%SomeType* %0) {
entry:
  %1 = getelementptr inbounds %SomeType, %SomeType* %0, i32 0, i32 0
  %2 = load i32, i32* %1
  ret i32 %2
}

define i16 @SomeType.unwrapB(%SomeType* %0) {
entry:
  %1 = getelementptr inbounds %SomeType, %SomeType* %0, i32 0, i32 1
  %2 = load i16, i16* %1
  ret i16 %2
}

define void @SomeType.addToB(%SomeType* %0, i16 %1) {
entry:
  %2 = getelementptr inbounds %SomeType, %SomeType* %0, i32 0, i32 1
  %3 = load i16, i16* %2
  %4 = add i16 %3, %1
  %5 = getelementptr inbounds %SomeType, %SomeType* %0, i32 0, i32 1
  store i16 %4, i16* %5
  ret void
}

define void @SomeType.consumeAsInfo(%SomeType* %0, %NonCopyType* %1) {
entry:
  %2 = getelementptr inbounds %SomeType, %SomeType* %0, i32 0, i32 2
  %3 = load %NonCopyType, %NonCopyType* %2
  store %NonCopyType %3, %NonCopyType* %1
  ret void
}

define void @SomeType.consumeAsInfoAndUpdateCache(%SomeType* %0, %NonCopyType* %1) {
entry:
  %2 = getelementptr inbounds %SomeType, %SomeType* %0, i32 0, i32 2
  call void @NonCopyType.updateCache(%NonCopyType* %2)
  %3 = getelementptr inbounds %SomeType, %SomeType* %0, i32 0, i32 2
  %4 = load %NonCopyType, %NonCopyType* %3
  store %NonCopyType %4, %NonCopyType* %1
  ret void
}

define i32 @SomeType.getWrongAnswer() {
entry:
  ret i32 41
}

"#;

#[test]
fn test() { common::test_single_file(SRC, RESULT) }
