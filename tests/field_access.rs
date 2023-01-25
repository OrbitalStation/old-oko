mod common;

const SRC: &str = r#"
ty Iter
	privateData: u8

ty Pred
	privateData: *()

ty Filter
	iter: Iter
	pred: Pred

	getIterPD.& = i.iter.privateData

	getPredPD.& = i.pred.privateData

ty A = b: Y.B
	ty B = c: Y.C
		ty C = d: Y.D
			ty D = e: Y.E
				ty E = f: Y.F
					ty F = finally: u8

unwrap a: A = a.b.c.d.e.f.finally

"#;

const RESULT: &str = r#"
; ModuleID = 'oko'
source_filename = "oko"

%Iter = type { i8 }
%.tuple. = type {}
%Pred = type { %.tuple.* }
%Filter = type { %Iter, %Pred }
%A.B = type { %A.B.C }
%A.B.C = type { %A.B.C.D }
%A.B.C.D = type { %A.B.C.D.E }
%A.B.C.D.E = type { %A.B.C.D.E.F }
%A.B.C.D.E.F = type { i8 }
%A = type { %A.B }

define void @Iter.new(i8 %0, %Iter* %1) {
entry:
  %2 = getelementptr inbounds %Iter, %Iter* %1, i32 0, i32 0
  store i8 %0, i8* %2
  ret void
}

define void @Pred.new(%.tuple.* %0, %Pred* %1) {
entry:
  %2 = getelementptr inbounds %Pred, %Pred* %1, i32 0, i32 0
  store %.tuple.* %0, %.tuple.** %2
  ret void
}

define i8 @Filter.getIterPD(%Filter* %0) {
entry:
  %1 = getelementptr inbounds %Filter, %Filter* %0, i32 0, i32 0
  %2 = getelementptr inbounds %Iter, %Iter* %1, i32 0, i32 0
  %3 = load i8, i8* %2
  ret i8 %3
}

define %.tuple.* @Filter.getPredPD(%Filter* %0) {
entry:
  %1 = getelementptr inbounds %Filter, %Filter* %0, i32 0, i32 1
  %2 = getelementptr inbounds %Pred, %Pred* %1, i32 0, i32 0
  %3 = load %.tuple.*, %.tuple.** %2
  ret %.tuple.* %3
}

define void @Filter.new(%Iter* %0, %Pred* %1, %Filter* %2) {
entry:
  %3 = getelementptr inbounds %Filter, %Filter* %2, i32 0, i32 0
  %4 = load %Iter, %Iter* %0
  store %Iter %4, %Iter* %3
  %5 = getelementptr inbounds %Filter, %Filter* %2, i32 0, i32 1
  %6 = load %Pred, %Pred* %1
  store %Pred %6, %Pred* %5
  ret void
}

define void @A.new(%A.B* %0, %A* %1) {
entry:
  %2 = getelementptr inbounds %A, %A* %1, i32 0, i32 0
  %3 = load %A.B, %A.B* %0
  store %A.B %3, %A.B* %2
  ret void
}

define void @A.B.new(%A.B.C* %0, %A.B* %1) {
entry:
  %2 = getelementptr inbounds %A.B, %A.B* %1, i32 0, i32 0
  %3 = load %A.B.C, %A.B.C* %0
  store %A.B.C %3, %A.B.C* %2
  ret void
}

define void @A.B.C.new(%A.B.C.D* %0, %A.B.C* %1) {
entry:
  %2 = getelementptr inbounds %A.B.C, %A.B.C* %1, i32 0, i32 0
  %3 = load %A.B.C.D, %A.B.C.D* %0
  store %A.B.C.D %3, %A.B.C.D* %2
  ret void
}

define void @A.B.C.D.new(%A.B.C.D.E* %0, %A.B.C.D* %1) {
entry:
  %2 = getelementptr inbounds %A.B.C.D, %A.B.C.D* %1, i32 0, i32 0
  %3 = load %A.B.C.D.E, %A.B.C.D.E* %0
  store %A.B.C.D.E %3, %A.B.C.D.E* %2
  ret void
}

define void @A.B.C.D.E.new(%A.B.C.D.E.F* %0, %A.B.C.D.E* %1) {
entry:
  %2 = getelementptr inbounds %A.B.C.D.E, %A.B.C.D.E* %1, i32 0, i32 0
  %3 = load %A.B.C.D.E.F, %A.B.C.D.E.F* %0
  store %A.B.C.D.E.F %3, %A.B.C.D.E.F* %2
  ret void
}

define void @A.B.C.D.E.F.new(i8 %0, %A.B.C.D.E.F* %1) {
entry:
  %2 = getelementptr inbounds %A.B.C.D.E.F, %A.B.C.D.E.F* %1, i32 0, i32 0
  store i8 %0, i8* %2
  ret void
}

define i8 @unwrap(%A* %0) {
entry:
  %1 = getelementptr inbounds %A, %A* %0, i32 0, i32 0
  %2 = getelementptr inbounds %A.B, %A.B* %1, i32 0, i32 0
  %3 = getelementptr inbounds %A.B.C, %A.B.C* %2, i32 0, i32 0
  %4 = getelementptr inbounds %A.B.C.D, %A.B.C.D* %3, i32 0, i32 0
  %5 = getelementptr inbounds %A.B.C.D.E, %A.B.C.D.E* %4, i32 0, i32 0
  %6 = getelementptr inbounds %A.B.C.D.E.F, %A.B.C.D.E.F* %5, i32 0, i32 0
  %7 = load i8, i8* %6
  ret i8 %7
}


"#;

#[test]
fn test() { common::test_single_file(SRC, RESULT) }
