(*
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
*)

let number = 600851475143L
let top = (int64 (sqrt (float number)))
let mutable values = [2L..top]
let mutable primes:int64 list = []
let halt = float top |> sqrt |> int

while values.Head < halt do
    primes <- values.Head::primes
    values <- List.filter(fun x -> x % values.Head <> 0) values
primes <- (List.rev primes) @ values
let answer = primes |> List.filter(fun x -> (number) % x = 0) |> List.max
printfn "Answer: %d" answer