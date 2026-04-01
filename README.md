# Parlang (par)

**par** is a toy programming language built in Scala. 

## Example

```par
// Tuple unbinding
let (first: Int, *rest: *Int) := (1, 2, 3, 4)

// Currying
let addCur(x: Int)(y: Int) -> Int := x + y

// Function with a tuple parameter
let addTup(x: Int, y: Int) -> Int := x + y

println((addCur 10 20) = addTup(10, 20))    // true

// Flat tuple
println((1, (2, 3)) = ((1, 2), 3))          // false

// Tagged tuple
println((1, #(2, 3)) = (#(1, 2), 3))        // true
```
