* Add `InvisibleTimes` character(⁢) to allow math-like multiplying
* Private functions are prefixed with `__`, the rest are public
* Allow not only `a.b.c` but also `(<any expr>).b.c`, i.e. any expr
    within brackets
* Add possibility to use custom macros to define an expression
    when a given type is expected.
    I.e. let's say there's an enum type `AVeryLongEnumName` and we want
    to match against a variable of this type in `choose`. We can do it like this:
    ```
    choose variable
        AVeryLongEnumName.VariantA => doSmth
        AVeryLongEnumName.VariantB => doSmth2
        ...
    ```
    But wouldn't it be cooler to do it like this instead?
    ```
    choose variable
        VariantA => doSmth
        VariantB => doSmth2
        ...
    ```
   I.e. no full-path qualifiers. Very cool!
   It shall also extend further than just enums. You get the point.
* Add `mutable` attribute for fields of struct that will allow modifying
    them through a shared reference; should be highly unsafe
* Generics in functions and structs are found in this way:
    if a type is a single uppercase letter then it is
    not treated like an undefined type but as a generic
* No surprise, but from the following item of this list consequences
    that a user-defined type is not allowed to consist of
    a single uppercase letter
* Forbid function overloading and only allow default values for args.
    Example:
    ```
    * Can omit type here, it is inferred automatically
    sayHello receiver="Joshuae"
        println "Hallo, $receiver!"
  
    * Hallo, Joshuae!
    sayHello
    
    * Hallo, Qyrpyt!
    sayHello receiver="Qyrpyt"
    ```
