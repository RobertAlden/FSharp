
let test = System.IO.File.ReadAllLines "test.txt" |> Array.toList
let input = System.IO.File.ReadAllLines "input.txt" |> Array.toList

let splitHalf str = 
    let half = String.length str / 2
    [str[0..half-1]; str[half..String.length str]]

let sharedChar strings =
    strings |> List.map (fun x -> x |> (Seq.toList >> Set.ofList)) 
            |> List.reduce (fun a b -> Set.intersect a b) 
            |> Set.toList
            
let alphaValue (ch: char) = 
    " abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" 
    |> Seq.toList 
    |> List.findIndex (fun x -> x = ch)
    
let silver data = data |> List.map splitHalf |> List.collect sharedChar |> List.map alphaValue |> List.sum
let gold data = data |> List.chunkBySize 3 |> List.collect sharedChar |> List.map alphaValue |> List.sum

printfn "--- TEST RUN ---"
printfn $"Silver: {silver test}"
printfn $"Gold: {gold test}"

printfn "--- ACTUAL RUN ---"
printfn $"Silver: {silver input}"
printfn $"Gold: {gold input}"