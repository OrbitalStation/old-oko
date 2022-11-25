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

* `TypeName` - a reference to the type of that name
* `*T` -> a constant pointer to the `T`
* `^T` -> a mutable pointer to the `T`
* `(X, Y, Z, ...)` -> a tuple of types

## Extern function definition

`puts str: *u8 -> i32 = extern`

## Variable definition
`x := expression`
