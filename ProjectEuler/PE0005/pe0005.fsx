(*
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
*)

let rec gcd (x:int64) (y:int64) = if y = 0 then abs x else gcd y (x % y)

let lcm x y = x * y / (gcd x y)

let answer = [1L..20] |> List.reduceBack(lcm)

printfn "Answer %d" answer