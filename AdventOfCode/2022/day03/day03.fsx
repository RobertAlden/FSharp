
let test = System.IO.File.ReadAllLines "test.txt" |> Array.toList
let input = System.IO.File.ReadAllLines "input.txt" |> Array.toList

let splitHalf str = 
    let half = String.length str / 2
    (str[0..half-1], str[half..String.length str])

let sharedChar (a, b) =
        a |> Seq.toList |> Set.ofList
        b |> Seq.toList |> Set.ofList

let silver data = data 
let gold data = data 

printfn "--- TEST RUN ---"
printfn $"Silver: {silver test}"
printfn $"Gold: {gold test}"

printfn "--- ACTUAL RUN ---"
printfn $"Silver: {silver input}"
printfn $"Gold: {gold input}"