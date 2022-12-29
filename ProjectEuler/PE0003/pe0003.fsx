(*
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
*)

let number = 600851475143L
let top = (int64 (sqrt (float number)))


let sieve (limit: int64) = 
    let halt = float limit |> sqrt |> int
    let rec collectPrime ((sieve: int64 list),(prime: int64 list)) = 
        if sieve.Head < halt then filterMultiples (sieve, prime @ [sieve.Head]) else prime @ sieve
    and filterMultiples ((sieve: int64 list),(prime: int64 list)) = 
        collectPrime((sieve |> List.filter(fun x -> x % sieve.Head <> 0)),prime)
    collectPrime ([2L..limit],[])

let primes = sieve top

let answer = primes |> List.filter(fun x -> (number) % x = 0) |> List.max
printfn $"Answer: {answer}"