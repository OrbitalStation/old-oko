## Comments
```
* Text
* ^ the beginning of the line with possibly some tabs before
* Note that a space after `*` is required to differentiate between
*   a comment and dereference
```

## Types definition

### Short form

#### Full enums

`enum bool = yes | no`

`enum optional = None | Some T`

#### Enums with one variant -- `new`

`ty Filter = iter: I + pred: P`

### Long form

#### Full enums

```
enum bool
    yes
    no
```

```
enum optional
    None
    Some T
```

#### Enums with one variant -- `new`

```
ty Filter
    iter: I
    pred: P
```

### Type aliases

`ty int = alias i32`

`ty ReferenceToI32 = alias &i32`

## Types' associated items

### Methods

```
ty Wrapper = x: i32
    unwrap.& = i.x
    
    new.* = ...
```

* `.&` - a reference to the type
* `.$` - a mutable reference
* `.!` - a moved value of the type
* `.$!` - a mutable moved value
* `.*` - a static method

## Function definition

### Short form

`add a b: i32 = a + b`

`pass = ()`

### Long form

```
add a b: i32 -> i32
    return a + b
```

```
drop x: T
    pass
```

## Types

* `Y` -> Rust's `Self`
* `TypeName` -> a type of that name
* `*T` -> a constant pointer to `T`
* `^T` -> a mutable pointer to `T`
* `(T)` -> the same as `T`
* `(X, Y, Z, ...)` -> a tuple of types
* `[T x 14]` -> an array of types
* `&T` -> a reference to `T`
* `$T` -> a mutable reference to `T`

Difference between pointers and references(in the future) is that
pointers are unsafe and fully explicit, whilst references are
restricted with lifetimes and usually allow implicit stuff

## Extern function definition

`puts str: *u8 -> i32 = extern`

## Variable definition
`x := expression`

## Mutable variable definition
`$x := expression`

## Assignment
`x = 3`
`x = y + z`

## Function call
`<fn name> <arg1> <arg2> ...`
`add 3 4`

`<fn name>(<arg1>, <arg2>, ...)`
`add(3, 4)`

## Shorthand assignment
`x += 3`
`x -= 2`

## Binary operators
```
a + b
x + y * z
(x + y) / z
q - p
a == b
a != b
a and b
a or b
```

## Field access
`structName.fieldName`
`a.b.c`
`wrapper.inner`

## Method call
`val.method`
`val.method()`

`val.method(x)`
`val.method x`

`val.method(1, 2, 34)`
`val.method 1 2 34`

`val.method1(1234).method2.method3(14, 12).method4.method5.field1.method6 2`

## Dereferencing
`*ptr`
`*a.y.x`
`*(expr)`

## Ifs
```
if <cond>
    <stmt1>
    <stmtN>

if <cond>
    <stmt1>
    <stmtN>
else
    <stmt1>
    <stmtN>

if <cond>
    <stmt1>
    <stmtN>
else if <cond2>
    <stmt1>
    <stmtN>
else
    <stmt1>
    <stmtN>

if <cond> do <stmt>

if <cond> do <stmt> else <stmt2>

if <cond>
    <stmt1>
    <stmtN>
else <stmt>
```

## Blocks
```
block
    <stmt1>
    <stmtN>
```
