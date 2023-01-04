* Add `InvisibleTimes` character(⁢) to allow math-like multiplication
* Private functions are prefixed with `__`, the rest are public
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
    For example, string literals are a form of this macro. `"Hi"`
    Macros of this type should also divide into strong and weak ones -
    Strong might be used anywhere(literals), whilst weak can be used
    Only where their type is expected.
    More examples of strong:
        <li>tuples(a, b)</li>
        <li>arrays([1 2 3 4])</li>
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
* Ability to call functions like so:
    ```
    sayHello receiver: String = println "Hallo there, $receiver!"
  
    * Hallo there, Her Majesty Gub-Gub!
    sayHello "Her Majesty Gub-Gub"
  
    complexCalculations -> String
        * Some complex calcs
        * ...
        * ...
        * ...
        * Finished
        return "Kirk"
  
    * Here, `complexCalculations` will be called only when
    *   is first used in the `sayHello` function.
    * The `lazy` function returns special `lazy` type,
    *   which does the above thing
    sayHello lazy &complexCalculations
    ```

* Switch meaning of `=` and `:=` in variables -- `=` would define a new
    variable, and `:=` will assign a value to it.
  Static variables will not be confused with functions because
    those variables require keyword `statik`.
  The only place where ambiguity might actually happen
    is in fun stmt context because `a = b` may mean both
    a variable `a` with the value of `b` or a function `a`
    that returns `b`. In these cases the preference is given
    to the assumption of a variable.
* Function pointer types are defined with λ, whilst
    lambdas themselves in Kotlin-style brackets -- `{ ... }`
* If some expr continues on the next line wih an extra tab it is
    treated as if it was on one line, i.e.
    ```
    binkie.winkie.dinkie.tinkie.pinkie
    * is treated the same as
    binkie.
        winkie.
        dinkie
        .tinkie
        .pinkie
    ```
* (This one is questionable)
    Add shared reference independence, i.e. ability to omit references
    whenever possible, such as field accessing - 
    if `a` is of type `&T` and `a.b` is non-copy,
    it's obvious that `a.b` is not trying to move `b` out
    of `a`, 
* Add appending of code right after the function to its body, i.e.
    ```
    * So that this code...
    someFun x: i64 = if x == 0
        ...
    else
        ...
  
    * ...would be treated like this
    someFun x: i64 = block
        if x == 0
            ...
        else
            ...
    ```
* Placeholder would be `...` instead of `_` of Rust, Python, etc.
* Add `typeof` operator
* Think about effects from `Koka`, maybe it will be possible to use them
