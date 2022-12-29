(*
Work out the first ten digits of the sum of the following one-hundred 50-digit numbers.
<see numbers.txt>
*)

let numbers = System.IO.File.ReadLines "numbers.txt" 
              |> Seq.toList 
              |> List.map(fun x -> System.Numerics.BigInteger.Parse x)

let answer = numbers |> List.sum |> string
printfn $"Answer: {answer[0..9]}"
