(*
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?
*)
let limit = 200000L

let rec primeIter (p:int64 list) (n: int64) = 
    if n >= limit then p 
    else (if p |> List.map(fun x -> n % x <> 0) |> List.reduce(&&) = false then
            (primeIter p (n + 2L)) else (primeIter (n::p) (n + 2L)))

let answer = primeIter [2] 3 |> List.rev |> List.item(10000)