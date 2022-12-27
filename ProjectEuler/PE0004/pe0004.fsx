(*
A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
*)

let palindrome n = 
    n = List.rev n

let rec digits n = 
    match n with
    | 0 -> []
    | _ -> digits (n/10) @ [n%10]

let product f a b = [for x in a do
                       for y in b do
                           yield f x y]

let range = [100..999]

let numbers = product (*) range range

let answer = numbers |> List.distinct
                     |> List.filter(fun x -> digits x |> palindrome)
                     |> List.max

printfn "Answer %d" answer

