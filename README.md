## Comments
```
* Text
* ^ the beginning of the line with possibly some tabs before
* Note that a space after `*` is required to differentiate between
*   a comment and dereference
```

## Types definition

### Enums

#### Short form

`ty bool = yes | no`

`ty optional = None | Some T`

#### Long form

```
ty bool
    yes
    no
```

```
ty optional
    None
    Some T
```

### Structs

#### Short form

`ty Filter = iter: Iter + pred: Pred`

#### Long form

```
ty Filter
    iter: Iter
    pred: Pred
```

### Opaques

`ty Name = opaque`

## Types' methods

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

`add a b: T = <expr>`

`pass = ()`

### Long form

```
add a b: T -> T
    return a + b
```

```
drop x: T
    pass
```

## Types

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

## Shorthand assignment
`x += 3`
`x -= 2`

## Binary operators
```
a + b
x + y × z
(x + y) ÷ z
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
