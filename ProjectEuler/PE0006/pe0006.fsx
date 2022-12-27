(*
The sum of the squares of the first ten natural numbers is,
        1^2 + 2^2 + ... 10^2 = 385

The square of the sum of the first ten natural numbers is,
        (1 + 2 + ... + 10)^2 = 55^2 = 3025

Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is:
        3025 - 385 = 2640

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
*)


let limit = 100

let sumOfSquares = List.sum [for i in [1..limit] -> i * i]

let squareOfSum = int ((List.reduce (+) [1f..(float32 limit)]) ** 2f)

let answer = squareOfSum - sumOfSquares

printfn "Answer %d" answer