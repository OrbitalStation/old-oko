mod common;

const SRC: &str = r#"
* Comment
pass = ()

* Comment * Comment

* ulalalalalalal
	* lala
					* lllllllllllllla
				* lasdaedfl
		* *

* ******************* *
*      With data      *
* ******************* *

* Empty comment vvvvvv
* haha, tricked ya. NOT ALLOWED.

* ************************** *

"#;

const RESULT: &str = r#"
; ModuleID = 'oko'
source_filename = "oko"

%.tuple. = type {}

define void @pass() {
entry:
  %0 = alloca %.tuple.
  ret void
}

"#;

#[test]
fn test() { common::test_single_file(SRC, RESULT) }
