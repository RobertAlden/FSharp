(*
    2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

    What is the sum of the digits of the number 2^1000?
*)

let answer = System.Numerics.BigInteger.Pow(2I,1000) 
                |> string 
                |> Seq.toList 
                |> List.map(fun x -> int64 x - 48L) 
                |> List.sum

printfn $"Answer: {answer}"