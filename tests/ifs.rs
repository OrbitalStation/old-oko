mod common;

const SRC: &str = r#"
ty int = alias i32

minusOne = 0 - 1

cmp a b: bool -> int
	return if a == b do 0 else if a do 1 else minusOne

toInt a b: bool -> int
    return if a
        if b
            3
        else
            2
    else
        if b
            1
        else
            0

fib n: u64 -> u64 = if n == 0 or n == 1 do 1 else (fib n - 1) + fib n - 2

fib2 n: u64 -> u64
	if n == 0
		if n == 1
			return 1
	a := fib n - 1
	b := fib n - 2
	return a + b

"#;

const RESULT: &str = r#"
; ModuleID = 'oko'
source_filename = "oko"

%.tuple. = type {}

define i32 @minusOne() {
entry:
  ret i32 -1
}

define i32 @cmp(i1 %0, i1 %1) {
entry:
  %2 = icmp eq i1 %0, %1
  br i1 %2, label %.yes, label %.no

.yes:                                             ; preds = %entry
  br label %.endif3

.no:                                              ; preds = %entry
  br i1 %0, label %.yes1, label %.no2

.yes1:                                            ; preds = %.no
  br label %.endif

.no2:                                             ; preds = %.no
  %3 = call i32 @minusOne()
  br label %.endif

.endif:                                           ; preds = %.no2, %.yes1
  %4 = phi i32 [ 1, %.yes1 ], [ %3, %.no2 ]
  br label %.endif3

.endif3:                                          ; preds = %.endif, %.yes
  %5 = phi i32 [ 0, %.yes ], [ %4, %.endif ]
  ret i32 %5
}

define i32 @toInt(i1 %0, i1 %1) {
entry:
  br i1 %0, label %.yes, label %.no2

.yes:                                             ; preds = %entry
  br i1 %1, label %.yes1, label %.no

.yes1:                                            ; preds = %.yes
  br label %.endif

.no:                                              ; preds = %.yes
  br label %.endif

.endif:                                           ; preds = %.no, %.yes1
  %2 = phi i32 [ 3, %.yes1 ], [ 2, %.no ]
  br label %.endif6

.no2:                                             ; preds = %entry
  br i1 %1, label %.yes3, label %.no4

.yes3:                                            ; preds = %.no2
  br label %.endif5

.no4:                                             ; preds = %.no2
  br label %.endif5

.endif5:                                          ; preds = %.no4, %.yes3
  %3 = phi i32 [ 1, %.yes3 ], [ 0, %.no4 ]
  br label %.endif6

.endif6:                                          ; preds = %.endif5, %.endif
  %4 = phi i32 [ %2, %.endif ], [ %3, %.endif5 ]
  ret i32 %4
}

define i64 @fib(i64 %0) {
entry:
  %1 = icmp eq i64 %0, 0
  %2 = icmp eq i64 %0, 1
  %3 = or i1 %1, %2
  br i1 %3, label %.yes, label %.no

.yes:                                             ; preds = %entry
  br label %.endif

.no:                                              ; preds = %entry
  %4 = sub i64 %0, 1
  %5 = call i64 @fib(i64 %4)
  %6 = sub i64 %0, 2
  %7 = call i64 @fib(i64 %6)
  %8 = add i64 %5, %7
  br label %.endif

.endif:                                           ; preds = %.no, %.yes
  %9 = phi i64 [ 1, %.yes ], [ %8, %.no ]
  ret i64 %9
}

define i64 @fib2(i64 %0) {
entry:
  %1 = icmp eq i64 %0, 0
  br i1 %1, label %.yes, label %.endif2

.yes:                                             ; preds = %entry
  %2 = icmp eq i64 %0, 1
  br i1 %2, label %.yes1, label %.endif

.yes1:                                            ; preds = %.yes
  ret i64 1

.endif:                                           ; preds = %.yes
  %3 = alloca %.tuple.
  br label %.endif2

.endif2:                                          ; preds = %.endif, %entry
  %4 = alloca %.tuple.
  %5 = sub i64 %0, 1
  %6 = call i64 @fib(i64 %5)
  %7 = sub i64 %0, 2
  %8 = call i64 @fib(i64 %7)
  %9 = add i64 %6, %8
  ret i64 %9
}


"#;

#[test]
fn test() { common::test_single_file(SRC, RESULT) }
