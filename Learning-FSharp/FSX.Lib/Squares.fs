module Squares

let square  i:int   = i * i
let squareF f:float = f * f

let sumOfSquares  i:int   = [1..i]   |> List.map square  |> List.sum
let sumOfSquaresF f:float = [1.0..f] |> List.map squareF |> List.sum

