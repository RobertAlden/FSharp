(*
    The four adjacent digits in the 1000-digit number that have the greatest product are 9 × 9 × 8 × 9 = 5832.

    <number in file>

    Find the thirteen adjacent digits in the 1000-digit number that have the greatest product. What is the value of this product?
*)

let path = "C:\\Users\\randy\\Documents\\Git Repos\\FSharp\\ProjectEuler\\PE0008\\number.txt"
let data = System.IO.File.ReadAllLines path 
           |> String.concat "" 
           |> Seq.toList 
           |> List.map(fun x -> int64 (string x)) 
           |> List.windowed 13 
           |> List.map(fun x -> x |> List.reduce(*)) 
           |> List.max
 
printfn "Answer: %d" data