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

%Filter = type { %Iter, %Pred }
%Iter = type { i8 }
%Pred = type { %.tuple.* }
%.tuple. = type {}
%A = type { %A.B }
%A.B = type { %A.B.C }
%A.B.C = type { %A.B.C.D }
%A.B.C.D = type { %A.B.C.D.E }
%A.B.C.D.E = type { %A.B.C.D.E.F }
%A.B.C.D.E.F = type { i8 }

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
