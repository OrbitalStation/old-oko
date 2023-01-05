mod common;

const SRC: &str = r#"
enum bool = yes | no
	id.& = i

enum bool2
	yes
	no

	id.& = i

enum TripleBool = yes | no | probably
	id.& = i

enum TripleBool2
	Yes
	No
	Probably

	id.& = i

enum SureBool = Sure
	id.& = i

enum SureBool2
	Sure

	id.& = i

enum NahBool = Nah
	id.& = i

enum NahBool2
	Nah

	id.& = i

* ******************* *
*      With data      *
* ******************* *

enum optional = None | Some i32
	id.& = i

enum optional2
	None
	Some i32

	id.& = i

enum result = Ok SureBool | Err NahBool
	id.& = i

enum result2
	Ok SureBool
	Err NahBool

	id.& = i

enum Position = TwoD x y: i32 | ThreeD x y z: i32 | FourD x y z w: i32
	id.& = i

enum Position2
	TwoD
		x: i32
		y: i32
	ThreeD x y z: i32
	FourD
		x y z: i32
		w: i32

	id.& = i

enum PositionTuple = NoD | TwoD i32 i32 | ThreeD i32 i32 i32 | FourD i32 i32 i32 i32
	id.& = i

enum PositionTuple2
	NoD
	TwoD i32 i32
	ThreeD i32 i32 i32
	FourD i32 i32 i32 i32

	id.& = i

"#;

const RESULT: &str = r#"
; ModuleID = 'oko'
source_filename = "oko"

%bool = type { i8 }
%bool2 = type { i8 }
%TripleBool = type { i8 }
%TripleBool2 = type { i8 }
%SureBool = type {}
%SureBool2 = type {}
%NahBool = type {}
%NahBool2 = type {}
%optional = type { i8, [4 x i8] }
%optional2 = type { i8, [4 x i8] }
%result = type { i8 }
%result2 = type { i8 }
%Position = type { i8, [16 x i8] }
%Position2 = type { i8, [16 x i8] }
%PositionTuple = type { i8, [16 x i8] }
%PositionTuple2 = type { i8, [16 x i8] }

define %bool* @bool.id(%bool* %0) {
entry:
  ret %bool* %0
}

define %bool2* @bool2.id(%bool2* %0) {
entry:
  ret %bool2* %0
}

define %TripleBool* @TripleBool.id(%TripleBool* %0) {
entry:
  ret %TripleBool* %0
}

define %TripleBool2* @TripleBool2.id(%TripleBool2* %0) {
entry:
  ret %TripleBool2* %0
}

define %SureBool* @SureBool.id(%SureBool* %0) {
entry:
  ret %SureBool* %0
}

define %SureBool2* @SureBool2.id(%SureBool2* %0) {
entry:
  ret %SureBool2* %0
}

define %NahBool* @NahBool.id(%NahBool* %0) {
entry:
  ret %NahBool* %0
}

define %NahBool2* @NahBool2.id(%NahBool2* %0) {
entry:
  ret %NahBool2* %0
}

define %optional* @optional.id(%optional* %0) {
entry:
  ret %optional* %0
}

define %optional2* @optional2.id(%optional2* %0) {
entry:
  ret %optional2* %0
}

define %result* @result.id(%result* %0) {
entry:
  ret %result* %0
}

define %result2* @result2.id(%result2* %0) {
entry:
  ret %result2* %0
}

define %Position* @Position.id(%Position* %0) {
entry:
  ret %Position* %0
}

define %Position2* @Position2.id(%Position2* %0) {
entry:
  ret %Position2* %0
}

define %PositionTuple* @PositionTuple.id(%PositionTuple* %0) {
entry:
  ret %PositionTuple* %0
}

define %PositionTuple2* @PositionTuple2.id(%PositionTuple2* %0) {
entry:
  ret %PositionTuple2* %0
}

"#;

#[test]
fn test() { common::test_single_file(SRC, RESULT) }
