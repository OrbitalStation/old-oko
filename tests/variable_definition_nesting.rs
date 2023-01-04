mod common;

const SRC: &str = r#"
one -> i32
    return block
        1

pass x: i32 = ()

sideEffect = 14

test
    x := sideEffect
    block
        if 1 == 1
            pass 0
        else
            pass 1
            pass x

test2
    x := sideEffect
    block
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
    block
        x := sideEffect
        pass x

test6
    block
        x := sideEffect
        block
            pass x

test7
    if 1 == 1
        x := sideEffect
        block
            block
                if 1 == 1
                    if 1 == 2
                        pass x
        pass x

"#;

const RESULT: &str = r#"
; ModuleID = 'oko'
source_filename = "oko"

%.tuple. = type {}

define i32 @one() {
entry:
  ret i32 1
}

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
  br i1 true, label %.yes, label %.no

.yes:                                             ; preds = %entry
  call void @pass(i32 0)
  br label %.endif

.no:                                              ; preds = %entry
  call void @pass(i32 1)
  call void @pass(i32 %0)
  br label %.endif

.endif:                                           ; preds = %.no, %.yes
  %1 = alloca %.tuple.
  ret void
}

define void @test2() {
entry:
  %0 = call i32 @sideEffect()
  br i1 true, label %.yes, label %.no

.yes:                                             ; preds = %entry
  call void @pass(i32 0)
  br label %.endif

.no:                                              ; preds = %entry
  call void @pass(i32 1)
  br label %.endif

.endif:                                           ; preds = %.no, %.yes
  %1 = alloca %.tuple.
  call void @pass(i32 %0)
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
  %0 = call i32 @sideEffect()
  call void @pass(i32 %0)
  ret void
}

define void @test6() {
entry:
  %0 = call i32 @sideEffect()
  call void @pass(i32 %0)
  ret void
}

define void @test7() {
entry:
  br i1 true, label %.yes, label %.endif4

.yes:                                             ; preds = %entry
  %0 = call i32 @sideEffect()
  br i1 true, label %.yes1, label %.endif3

.yes1:                                            ; preds = %.yes
  br i1 false, label %.yes2, label %.endif

.yes2:                                            ; preds = %.yes1
  call void @pass(i32 %0)
  br label %.endif

.endif:                                           ; preds = %.yes2, %.yes1
  %1 = alloca %.tuple.
  br label %.endif3

.endif3:                                          ; preds = %.endif, %.yes
  %2 = alloca %.tuple.
  call void @pass(i32 %0)
  br label %.endif4

.endif4:                                          ; preds = %.endif3, %entry
  %3 = alloca %.tuple.
  ret void
}


"#;

#[test]
fn test() { common::test_single_file(SRC, RESULT) }
