mod common;

const SRC: &str = r#"
pass x: i32 = ()

sideEffect = 14

test
    x := sideEffect
    if 1 == 1
        if 1 == 1
            pass 0
        else
            pass 1
            pass x

test2
    x := sideEffect
    if 1 == 1
        if 1 == 1
            pass 0
        else
            pass 1
        pass x

test3
    x := sideEffect
    pass x

test4
    x := sideEffect
    if 1 == 1
        pass x

test5
    if 1 == 1
        x := sideEffect
        pass x

test6
    if 1 == 1
        x := sideEffect
        if 1 == 1
            pass x

test7
    if 1 == 1
        x := sideEffect
        if 1 == 1
            if 1 == 1
                if 1 == 1
                    if 1 == 2
                        pass x
        pass x

"#;

const RESULT: &str = r#"
; ModuleID = 'oko'
source_filename = "oko"

%.tuple. = type {}

define void @pass(i32 %0) {
entry:
  %1 = alloca %.tuple.
  ret void
}

define i32 @sideEffect() {
entry:
  ret i32 14
}

define void @test() {
entry:
  %0 = call i32 @sideEffect()
  br i1 true, label %.yes, label %.endif2

.yes:                                             ; preds = %entry
  br i1 true, label %.yes1, label %.no

.yes1:                                            ; preds = %.yes
  call void @pass(i32 0)
  br label %.endif

.no:                                              ; preds = %.yes
  call void @pass(i32 1)
  call void @pass(i32 %0)
  br label %.endif

.endif:                                           ; preds = %.no, %.yes1
  %1 = alloca %.tuple.
  br label %.endif2

.endif2:                                          ; preds = %.endif, %entry
  %2 = alloca %.tuple.
  ret void
}

define void @test2() {
entry:
  %0 = call i32 @sideEffect()
  br i1 true, label %.yes, label %.endif2

.yes:                                             ; preds = %entry
  br i1 true, label %.yes1, label %.no

.yes1:                                            ; preds = %.yes
  call void @pass(i32 0)
  br label %.endif

.no:                                              ; preds = %.yes
  call void @pass(i32 1)
  br label %.endif

.endif:                                           ; preds = %.no, %.yes1
  %1 = alloca %.tuple.
  call void @pass(i32 %0)
  br label %.endif2

.endif2:                                          ; preds = %.endif, %entry
  %2 = alloca %.tuple.
  ret void
}

define void @test3() {
entry:
  %0 = call i32 @sideEffect()
  call void @pass(i32 %0)
  ret void
}

define void @test4() {
entry:
  %0 = call i32 @sideEffect()
  br i1 true, label %.yes, label %.endif

.yes:                                             ; preds = %entry
  call void @pass(i32 %0)
  br label %.endif

.endif:                                           ; preds = %.yes, %entry
  %1 = alloca %.tuple.
  ret void
}

define void @test5() {
entry:
  br i1 true, label %.yes, label %.endif

.yes:                                             ; preds = %entry
  %0 = call i32 @sideEffect()
  call void @pass(i32 %0)
  br label %.endif

.endif:                                           ; preds = %.yes, %entry
  %1 = alloca %.tuple.
  ret void
}

define void @test6() {
entry:
  br i1 true, label %.yes, label %.endif2

.yes:                                             ; preds = %entry
  %0 = call i32 @sideEffect()
  br i1 true, label %.yes1, label %.endif

.yes1:                                            ; preds = %.yes
  call void @pass(i32 %0)
  br label %.endif

.endif:                                           ; preds = %.yes1, %.yes
  %1 = alloca %.tuple.
  br label %.endif2

.endif2:                                          ; preds = %.endif, %entry
  %2 = alloca %.tuple.
  ret void
}

define void @test7() {
entry:
  br i1 true, label %.yes, label %.endif8

.yes:                                             ; preds = %entry
  %0 = call i32 @sideEffect()
  br i1 true, label %.yes1, label %.endif7

.yes1:                                            ; preds = %.yes
  br i1 true, label %.yes2, label %.endif6

.yes2:                                            ; preds = %.yes1
  br i1 true, label %.yes3, label %.endif5

.yes3:                                            ; preds = %.yes2
  br i1 false, label %.yes4, label %.endif

.yes4:                                            ; preds = %.yes3
  call void @pass(i32 %0)
  br label %.endif

.endif:                                           ; preds = %.yes4, %.yes3
  %1 = alloca %.tuple.
  br label %.endif5

.endif5:                                          ; preds = %.endif, %.yes2
  %2 = alloca %.tuple.
  br label %.endif6

.endif6:                                          ; preds = %.endif5, %.yes1
  %3 = alloca %.tuple.
  br label %.endif7

.endif7:                                          ; preds = %.endif6, %.yes
  %4 = alloca %.tuple.
  call void @pass(i32 %0)
  br label %.endif8

.endif8:                                          ; preds = %.endif7, %entry
  %5 = alloca %.tuple.
  ret void
}


"#;

#[test]
fn test() { common::test_single_file(SRC, RESULT) }
