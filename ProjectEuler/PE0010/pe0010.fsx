(*
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
*)

let sieve (limit: int64) = 
    let halt = float limit |> sqrt |> int
    let rec collectPrime ((sieve: int64 list),(prime: int64 list)) = 
        if sieve.Head < halt then filterMultiples (sieve, prime @ [sieve.Head]) else prime @ sieve
    and filterMultiples ((sieve: int64 list),(prime: int64 list)) = 
        collectPrime((sieve |> List.filter(fun x -> x % sieve.Head <> 0)),prime)
    collectPrime ([2L..limit],[])

let answer = sieve 2_000_000 |> List.sum