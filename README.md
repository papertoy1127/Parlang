# Parlang (par)

**par** is a toy programming language built in Scala. 

## Examples

```swift
// Tuple unbinding
let (first: Int, *rest: *Int) := (1, 2, 3, 4)

// Currying
let addCur(x: Int)(y: Int) -> Int := x + y

// Currying With Lambda
let addCurLambda := (x: Int) => (y: Int) => x + y

// Function with a tuple parameter
let addTup(x: Int, y: Int) -> Int := x + y

// Lambda with a tuple parameter
let addTupLambda := (x: Int, y: Int) => x + y

// addCur(10, 20)     --- Error (Expected single atom, got tuple)
// addTup 10 20       --- Error (Length mismatch)

(addCur 10 20) = addTup(10, 20)     // true

// Flat tuple
1, (2, 3) = (1, 2), 3               // true

// Tagged tuple
(1, #(2, 3)) = (#(1, 2), 3)         // false

let head (x: Int, *rest: *Int) -> Int := x
let tail (x: Int, *rest: *Int) -> Int := rest
let sum(*nums: *Int) -> Int := len nums ? head nums + sum(tail nums) : 0

sum(1, 2, 3)                        // 6
```