let test = System.IO.File.ReadAllLines "test.txt" |> Array.toList |> List.head
let input = System.IO.File.ReadAllLines "input.txt" |> Array.toList |> List.head

let searchForUniqueCharSubstringEnd len str =
    str |> Seq.toList 
        |> List.windowed len
        |> List.map (fun x -> Set.ofList x |> Set.count) 
        |> List.findIndex (fun x -> x = len)
        |> (+) len

let silver data = searchForUniqueCharSubstringEnd 4 data
                  
let gold data = searchForUniqueCharSubstringEnd 14 data

printfn "--- TEST RUN ---"
printfn $"Silver: {silver test}"
printfn $"Gold: {gold test}"

printfn "--- ACTUAL RUN ---"
printfn $"Silver: {silver input}"
printfn $"Gold: {gold input}"