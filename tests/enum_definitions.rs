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

%bool = type { i8, [0 x i8] }
%bool2 = type { i8, [0 x i8] }
%TripleBool = type { i8, [0 x i8] }
%TripleBool2 = type { i8, [0 x i8] }
%SureBool = type {}
%SureBool2 = type {}
%NahBool = type {}
%NahBool2 = type {}
%optional = type { i8, [4 x i8] }
%.tuple.i32 = type { i32 }
%optional2 = type { i8, [4 x i8] }
%result = type { i8, [0 x i8] }
%.tuple.SureBool = type { %SureBool }
%.tuple.NahBool = type { %NahBool }
%result2 = type { i8, [0 x i8] }
%Position = type { i8, [16 x i8] }
%.E.Position.TwoD = type { i32, i32 }
%.E.Position.ThreeD = type { i32, i32, i32 }
%.E.Position.FourD = type { i32, i32, i32, i32 }
%Position2 = type { i8, [16 x i8] }
%.E.Position2.TwoD = type { i32, i32 }
%.E.Position2.ThreeD = type { i32, i32, i32 }
%.E.Position2.FourD = type { i32, i32, i32, i32 }
%PositionTuple = type { i8, [16 x i8] }
%.tuple.i32-i32 = type { i32, i32 }
%.tuple.i32-i32-i32 = type { i32, i32, i32 }
%.tuple.i32-i32-i32-i32 = type { i32, i32, i32, i32 }
%PositionTuple2 = type { i8, [16 x i8] }

define %bool* @bool.id(%bool* %0) {
entry:
  ret %bool* %0
}

define void @bool.yes(%bool* %0) {
entry:
  %1 = getelementptr inbounds %bool, %bool* %0, i32 0, i32 0
  store i8 0, i8* %1
  ret void
}

define void @bool.no(%bool* %0) {
entry:
  %1 = getelementptr inbounds %bool, %bool* %0, i32 0, i32 0
  store i8 1, i8* %1
  ret void
}

define %bool2* @bool2.id(%bool2* %0) {
entry:
  ret %bool2* %0
}

define void @bool2.yes(%bool2* %0) {
entry:
  %1 = getelementptr inbounds %bool2, %bool2* %0, i32 0, i32 0
  store i8 0, i8* %1
  ret void
}

define void @bool2.no(%bool2* %0) {
entry:
  %1 = getelementptr inbounds %bool2, %bool2* %0, i32 0, i32 0
  store i8 1, i8* %1
  ret void
}

define %TripleBool* @TripleBool.id(%TripleBool* %0) {
entry:
  ret %TripleBool* %0
}

define void @TripleBool.yes(%TripleBool* %0) {
entry:
  %1 = getelementptr inbounds %TripleBool, %TripleBool* %0, i32 0, i32 0
  store i8 0, i8* %1
  ret void
}

define void @TripleBool.no(%TripleBool* %0) {
entry:
  %1 = getelementptr inbounds %TripleBool, %TripleBool* %0, i32 0, i32 0
  store i8 1, i8* %1
  ret void
}

define void @TripleBool.probably(%TripleBool* %0) {
entry:
  %1 = getelementptr inbounds %TripleBool, %TripleBool* %0, i32 0, i32 0
  store i8 2, i8* %1
  ret void
}

define %TripleBool2* @TripleBool2.id(%TripleBool2* %0) {
entry:
  ret %TripleBool2* %0
}

define void @TripleBool2.Yes(%TripleBool2* %0) {
entry:
  %1 = getelementptr inbounds %TripleBool2, %TripleBool2* %0, i32 0, i32 0
  store i8 0, i8* %1
  ret void
}

define void @TripleBool2.No(%TripleBool2* %0) {
entry:
  %1 = getelementptr inbounds %TripleBool2, %TripleBool2* %0, i32 0, i32 0
  store i8 1, i8* %1
  ret void
}

define void @TripleBool2.Probably(%TripleBool2* %0) {
entry:
  %1 = getelementptr inbounds %TripleBool2, %TripleBool2* %0, i32 0, i32 0
  store i8 2, i8* %1
  ret void
}

define %SureBool* @SureBool.id(%SureBool* %0) {
entry:
  ret %SureBool* %0
}

define void @SureBool.Sure(%SureBool* %0) {
entry:
  ret void
}

define %SureBool2* @SureBool2.id(%SureBool2* %0) {
entry:
  ret %SureBool2* %0
}

define void @SureBool2.Sure(%SureBool2* %0) {
entry:
  ret void
}

define %NahBool* @NahBool.id(%NahBool* %0) {
entry:
  ret %NahBool* %0
}

define void @NahBool.Nah(%NahBool* %0) {
entry:
  ret void
}

define %NahBool2* @NahBool2.id(%NahBool2* %0) {
entry:
  ret %NahBool2* %0
}

define void @NahBool2.Nah(%NahBool2* %0) {
entry:
  ret void
}

define %optional* @optional.id(%optional* %0) {
entry:
  ret %optional* %0
}

define void @optional.None(%optional* %0) {
entry:
  %1 = getelementptr inbounds %optional, %optional* %0, i32 0, i32 0
  store i8 0, i8* %1
  ret void
}

define void @optional.Some(i32 %0, %optional* %1) {
entry:
  %2 = getelementptr inbounds %optional, %optional* %1, i32 0, i32 0
  store i8 1, i8* %2
  %3 = getelementptr inbounds %optional, %optional* %1, i32 0, i32 1
  %4 = bitcast [4 x i8]* %3 to %.tuple.i32*
  %5 = getelementptr inbounds %.tuple.i32, %.tuple.i32* %4, i32 0, i32 0
  store i32 %0, i32* %5
  ret void
}

define %optional2* @optional2.id(%optional2* %0) {
entry:
  ret %optional2* %0
}

define void @optional2.None(%optional2* %0) {
entry:
  %1 = getelementptr inbounds %optional2, %optional2* %0, i32 0, i32 0
  store i8 0, i8* %1
  ret void
}

define void @optional2.Some(i32 %0, %optional2* %1) {
entry:
  %2 = getelementptr inbounds %optional2, %optional2* %1, i32 0, i32 0
  store i8 1, i8* %2
  %3 = getelementptr inbounds %optional2, %optional2* %1, i32 0, i32 1
  %4 = bitcast [4 x i8]* %3 to %.tuple.i32*
  %5 = getelementptr inbounds %.tuple.i32, %.tuple.i32* %4, i32 0, i32 0
  store i32 %0, i32* %5
  ret void
}

define %result* @result.id(%result* %0) {
entry:
  ret %result* %0
}

define void @result.Ok(%SureBool* %0, %result* %1) {
entry:
  %2 = getelementptr inbounds %result, %result* %1, i32 0, i32 0
  store i8 0, i8* %2
  %3 = getelementptr inbounds %result, %result* %1, i32 0, i32 1
  %4 = bitcast [0 x i8]* %3 to %.tuple.SureBool*
  %5 = getelementptr inbounds %.tuple.SureBool, %.tuple.SureBool* %4, i32 0, i32 0
  %6 = load %SureBool, %SureBool* %0
  store %SureBool %6, %SureBool* %5
  ret void
}

define void @result.Err(%NahBool* %0, %result* %1) {
entry:
  %2 = getelementptr inbounds %result, %result* %1, i32 0, i32 0
  store i8 1, i8* %2
  %3 = getelementptr inbounds %result, %result* %1, i32 0, i32 1
  %4 = bitcast [0 x i8]* %3 to %.tuple.NahBool*
  %5 = getelementptr inbounds %.tuple.NahBool, %.tuple.NahBool* %4, i32 0, i32 0
  %6 = load %NahBool, %NahBool* %0
  store %NahBool %6, %NahBool* %5
  ret void
}

define %result2* @result2.id(%result2* %0) {
entry:
  ret %result2* %0
}

define void @result2.Ok(%SureBool* %0, %result2* %1) {
entry:
  %2 = getelementptr inbounds %result2, %result2* %1, i32 0, i32 0
  store i8 0, i8* %2
  %3 = getelementptr inbounds %result2, %result2* %1, i32 0, i32 1
  %4 = bitcast [0 x i8]* %3 to %.tuple.SureBool*
  %5 = getelementptr inbounds %.tuple.SureBool, %.tuple.SureBool* %4, i32 0, i32 0
  %6 = load %SureBool, %SureBool* %0
  store %SureBool %6, %SureBool* %5
  ret void
}

define void @result2.Err(%NahBool* %0, %result2* %1) {
entry:
  %2 = getelementptr inbounds %result2, %result2* %1, i32 0, i32 0
  store i8 1, i8* %2
  %3 = getelementptr inbounds %result2, %result2* %1, i32 0, i32 1
  %4 = bitcast [0 x i8]* %3 to %.tuple.NahBool*
  %5 = getelementptr inbounds %.tuple.NahBool, %.tuple.NahBool* %4, i32 0, i32 0
  %6 = load %NahBool, %NahBool* %0
  store %NahBool %6, %NahBool* %5
  ret void
}

define %Position* @Position.id(%Position* %0) {
entry:
  ret %Position* %0
}

define void @Position.TwoD(i32 %0, i32 %1, %Position* %2) {
entry:
  %3 = getelementptr inbounds %Position, %Position* %2, i32 0, i32 0
  store i8 0, i8* %3
  %4 = getelementptr inbounds %Position, %Position* %2, i32 0, i32 1
  %5 = bitcast [16 x i8]* %4 to %.E.Position.TwoD*
  %6 = getelementptr inbounds %.E.Position.TwoD, %.E.Position.TwoD* %5, i32 0, i32 0
  store i32 %0, i32* %6
  %7 = getelementptr inbounds %.E.Position.TwoD, %.E.Position.TwoD* %5, i32 0, i32 1
  store i32 %1, i32* %7
  ret void
}

define void @Position.ThreeD(i32 %0, i32 %1, i32 %2, %Position* %3) {
entry:
  %4 = getelementptr inbounds %Position, %Position* %3, i32 0, i32 0
  store i8 1, i8* %4
  %5 = getelementptr inbounds %Position, %Position* %3, i32 0, i32 1
  %6 = bitcast [16 x i8]* %5 to %.E.Position.ThreeD*
  %7 = getelementptr inbounds %.E.Position.ThreeD, %.E.Position.ThreeD* %6, i32 0, i32 0
  store i32 %0, i32* %7
  %8 = getelementptr inbounds %.E.Position.ThreeD, %.E.Position.ThreeD* %6, i32 0, i32 1
  store i32 %1, i32* %8
  %9 = getelementptr inbounds %.E.Position.ThreeD, %.E.Position.ThreeD* %6, i32 0, i32 2
  store i32 %2, i32* %9
  ret void
}

define void @Position.FourD(i32 %0, i32 %1, i32 %2, i32 %3, %Position* %4) {
entry:
  %5 = getelementptr inbounds %Position, %Position* %4, i32 0, i32 0
  store i8 2, i8* %5
  %6 = getelementptr inbounds %Position, %Position* %4, i32 0, i32 1
  %7 = bitcast [16 x i8]* %6 to %.E.Position.FourD*
  %8 = getelementptr inbounds %.E.Position.FourD, %.E.Position.FourD* %7, i32 0, i32 0
  store i32 %0, i32* %8
  %9 = getelementptr inbounds %.E.Position.FourD, %.E.Position.FourD* %7, i32 0, i32 1
  store i32 %1, i32* %9
  %10 = getelementptr inbounds %.E.Position.FourD, %.E.Position.FourD* %7, i32 0, i32 2
  store i32 %2, i32* %10
  %11 = getelementptr inbounds %.E.Position.FourD, %.E.Position.FourD* %7, i32 0, i32 3
  store i32 %3, i32* %11
  ret void
}

define %Position2* @Position2.id(%Position2* %0) {
entry:
  ret %Position2* %0
}

define void @Position2.TwoD(i32 %0, i32 %1, %Position2* %2) {
entry:
  %3 = getelementptr inbounds %Position2, %Position2* %2, i32 0, i32 0
  store i8 0, i8* %3
  %4 = getelementptr inbounds %Position2, %Position2* %2, i32 0, i32 1
  %5 = bitcast [16 x i8]* %4 to %.E.Position2.TwoD*
  %6 = getelementptr inbounds %.E.Position2.TwoD, %.E.Position2.TwoD* %5, i32 0, i32 0
  store i32 %0, i32* %6
  %7 = getelementptr inbounds %.E.Position2.TwoD, %.E.Position2.TwoD* %5, i32 0, i32 1
  store i32 %1, i32* %7
  ret void
}

define void @Position2.ThreeD(i32 %0, i32 %1, i32 %2, %Position2* %3) {
entry:
  %4 = getelementptr inbounds %Position2, %Position2* %3, i32 0, i32 0
  store i8 1, i8* %4
  %5 = getelementptr inbounds %Position2, %Position2* %3, i32 0, i32 1
  %6 = bitcast [16 x i8]* %5 to %.E.Position2.ThreeD*
  %7 = getelementptr inbounds %.E.Position2.ThreeD, %.E.Position2.ThreeD* %6, i32 0, i32 0
  store i32 %0, i32* %7
  %8 = getelementptr inbounds %.E.Position2.ThreeD, %.E.Position2.ThreeD* %6, i32 0, i32 1
  store i32 %1, i32* %8
  %9 = getelementptr inbounds %.E.Position2.ThreeD, %.E.Position2.ThreeD* %6, i32 0, i32 2
  store i32 %2, i32* %9
  ret void
}

define void @Position2.FourD(i32 %0, i32 %1, i32 %2, i32 %3, %Position2* %4) {
entry:
  %5 = getelementptr inbounds %Position2, %Position2* %4, i32 0, i32 0
  store i8 2, i8* %5
  %6 = getelementptr inbounds %Position2, %Position2* %4, i32 0, i32 1
  %7 = bitcast [16 x i8]* %6 to %.E.Position2.FourD*
  %8 = getelementptr inbounds %.E.Position2.FourD, %.E.Position2.FourD* %7, i32 0, i32 0
  store i32 %0, i32* %8
  %9 = getelementptr inbounds %.E.Position2.FourD, %.E.Position2.FourD* %7, i32 0, i32 1
  store i32 %1, i32* %9
  %10 = getelementptr inbounds %.E.Position2.FourD, %.E.Position2.FourD* %7, i32 0, i32 2
  store i32 %2, i32* %10
  %11 = getelementptr inbounds %.E.Position2.FourD, %.E.Position2.FourD* %7, i32 0, i32 3
  store i32 %3, i32* %11
  ret void
}

define %PositionTuple* @PositionTuple.id(%PositionTuple* %0) {
entry:
  ret %PositionTuple* %0
}

define void @PositionTuple.NoD(%PositionTuple* %0) {
entry:
  %1 = getelementptr inbounds %PositionTuple, %PositionTuple* %0, i32 0, i32 0
  store i8 0, i8* %1
  ret void
}

define void @PositionTuple.TwoD(i32 %0, i32 %1, %PositionTuple* %2) {
entry:
  %3 = getelementptr inbounds %PositionTuple, %PositionTuple* %2, i32 0, i32 0
  store i8 1, i8* %3
  %4 = getelementptr inbounds %PositionTuple, %PositionTuple* %2, i32 0, i32 1
  %5 = bitcast [16 x i8]* %4 to %.tuple.i32-i32*
  %6 = getelementptr inbounds %.tuple.i32-i32, %.tuple.i32-i32* %5, i32 0, i32 0
  store i32 %0, i32* %6
  %7 = getelementptr inbounds %.tuple.i32-i32, %.tuple.i32-i32* %5, i32 0, i32 1
  store i32 %1, i32* %7
  ret void
}

define void @PositionTuple.ThreeD(i32 %0, i32 %1, i32 %2, %PositionTuple* %3) {
entry:
  %4 = getelementptr inbounds %PositionTuple, %PositionTuple* %3, i32 0, i32 0
  store i8 2, i8* %4
  %5 = getelementptr inbounds %PositionTuple, %PositionTuple* %3, i32 0, i32 1
  %6 = bitcast [16 x i8]* %5 to %.tuple.i32-i32-i32*
  %7 = getelementptr inbounds %.tuple.i32-i32-i32, %.tuple.i32-i32-i32* %6, i32 0, i32 0
  store i32 %0, i32* %7
  %8 = getelementptr inbounds %.tuple.i32-i32-i32, %.tuple.i32-i32-i32* %6, i32 0, i32 1
  store i32 %1, i32* %8
  %9 = getelementptr inbounds %.tuple.i32-i32-i32, %.tuple.i32-i32-i32* %6, i32 0, i32 2
  store i32 %2, i32* %9
  ret void
}

define void @PositionTuple.FourD(i32 %0, i32 %1, i32 %2, i32 %3, %PositionTuple* %4) {
entry:
  %5 = getelementptr inbounds %PositionTuple, %PositionTuple* %4, i32 0, i32 0
  store i8 3, i8* %5
  %6 = getelementptr inbounds %PositionTuple, %PositionTuple* %4, i32 0, i32 1
  %7 = bitcast [16 x i8]* %6 to %.tuple.i32-i32-i32-i32*
  %8 = getelementptr inbounds %.tuple.i32-i32-i32-i32, %.tuple.i32-i32-i32-i32* %7, i32 0, i32 0
  store i32 %0, i32* %8
  %9 = getelementptr inbounds %.tuple.i32-i32-i32-i32, %.tuple.i32-i32-i32-i32* %7, i32 0, i32 1
  store i32 %1, i32* %9
  %10 = getelementptr inbounds %.tuple.i32-i32-i32-i32, %.tuple.i32-i32-i32-i32* %7, i32 0, i32 2
  store i32 %2, i32* %10
  %11 = getelementptr inbounds %.tuple.i32-i32-i32-i32, %.tuple.i32-i32-i32-i32* %7, i32 0, i32 3
  store i32 %3, i32* %11
  ret void
}

define %PositionTuple2* @PositionTuple2.id(%PositionTuple2* %0) {
entry:
  ret %PositionTuple2* %0
}

define void @PositionTuple2.NoD(%PositionTuple2* %0) {
entry:
  %1 = getelementptr inbounds %PositionTuple2, %PositionTuple2* %0, i32 0, i32 0
  store i8 0, i8* %1
  ret void
}

define void @PositionTuple2.TwoD(i32 %0, i32 %1, %PositionTuple2* %2) {
entry:
  %3 = getelementptr inbounds %PositionTuple2, %PositionTuple2* %2, i32 0, i32 0
  store i8 1, i8* %3
  %4 = getelementptr inbounds %PositionTuple2, %PositionTuple2* %2, i32 0, i32 1
  %5 = bitcast [16 x i8]* %4 to %.tuple.i32-i32*
  %6 = getelementptr inbounds %.tuple.i32-i32, %.tuple.i32-i32* %5, i32 0, i32 0
  store i32 %0, i32* %6
  %7 = getelementptr inbounds %.tuple.i32-i32, %.tuple.i32-i32* %5, i32 0, i32 1
  store i32 %1, i32* %7
  ret void
}

define void @PositionTuple2.ThreeD(i32 %0, i32 %1, i32 %2, %PositionTuple2* %3) {
entry:
  %4 = getelementptr inbounds %PositionTuple2, %PositionTuple2* %3, i32 0, i32 0
  store i8 2, i8* %4
  %5 = getelementptr inbounds %PositionTuple2, %PositionTuple2* %3, i32 0, i32 1
  %6 = bitcast [16 x i8]* %5 to %.tuple.i32-i32-i32*
  %7 = getelementptr inbounds %.tuple.i32-i32-i32, %.tuple.i32-i32-i32* %6, i32 0, i32 0
  store i32 %0, i32* %7
  %8 = getelementptr inbounds %.tuple.i32-i32-i32, %.tuple.i32-i32-i32* %6, i32 0, i32 1
  store i32 %1, i32* %8
  %9 = getelementptr inbounds %.tuple.i32-i32-i32, %.tuple.i32-i32-i32* %6, i32 0, i32 2
  store i32 %2, i32* %9
  ret void
}

define void @PositionTuple2.FourD(i32 %0, i32 %1, i32 %2, i32 %3, %PositionTuple2* %4) {
entry:
  %5 = getelementptr inbounds %PositionTuple2, %PositionTuple2* %4, i32 0, i32 0
  store i8 3, i8* %5
  %6 = getelementptr inbounds %PositionTuple2, %PositionTuple2* %4, i32 0, i32 1
  %7 = bitcast [16 x i8]* %6 to %.tuple.i32-i32-i32-i32*
  %8 = getelementptr inbounds %.tuple.i32-i32-i32-i32, %.tuple.i32-i32-i32-i32* %7, i32 0, i32 0
  store i32 %0, i32* %8
  %9 = getelementptr inbounds %.tuple.i32-i32-i32-i32, %.tuple.i32-i32-i32-i32* %7, i32 0, i32 1
  store i32 %1, i32* %9
  %10 = getelementptr inbounds %.tuple.i32-i32-i32-i32, %.tuple.i32-i32-i32-i32* %7, i32 0, i32 2
  store i32 %2, i32* %10
  %11 = getelementptr inbounds %.tuple.i32-i32-i32-i32, %.tuple.i32-i32-i32-i32* %7, i32 0, i32 3
  store i32 %3, i32* %11
  ret void
}


"#;

#[test]
fn test() { common::test_single_file(SRC, RESULT) }
