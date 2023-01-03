
let test = System.IO.File.ReadAllLines "test.txt" |> Array.toList
let input = System.IO.File.ReadAllLines "input.txt" |> Array.toList

let rec partionByValue (sep: 'a) (lst: 'a list) = 
    match List.tryFindIndex(fun x -> x = sep) lst with
    | Some x -> List.splitAt x lst |> (fun (a,b) -> a::(partionByValue sep b.Tail))
    | None -> [lst]

let silver data = data |> partionByValue ""  
                       |> List.map(fun x -> List.map int x |> List.sum) 
                       |> List.max

let gold data = data |> partionByValue ""  
                     |> List.map(fun x -> List.map int x |> List.sum) 
                     |> List.sort |> List.rev
                     |> List.take 3
                     |> List.sum

printfn "--- TEST RUN ---"
printfn $"Silver: {silver test}"
printfn $"Gold: {gold test}"

printfn "--- ACTUAL RUN ---"
printfn $"Silver: {silver input}"
printfn $"Gold: {gold input}"