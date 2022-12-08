## Comments
`* Text`

`^ the beginning of the line with possibly some tabs before`

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
