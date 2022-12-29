(*
The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
*)


let collatz (n: int64) = 
    match n with
    | 1L -> 1L
    | _ -> [|n/2L; 3L*n+1L|][int (n%2L)]

let rec makeSequence f (v: int64 list) = 
    let nextV = f v.Head
    if nextV <> v.Head then
        makeSequence f (nextV::v)
    else
        v

let answer = [for i in [1L..999_999] -> makeSequence collatz [i] |> List.length] 
                |> List.indexed 
                |> List.maxBy (fun (i, v) -> v)
                |> (fun (i, v) -> i)

printfn $"Answer: {answer}"
        