
let test = System.IO.File.ReadAllLines "test.txt"
let input = System.IO.File.ReadAllLines "input.txt"

let scorePart1 str =
    match str with
    | "A X" -> 3 + 1
    | "B X" -> 0 + 1
    | "C X" -> 6 + 1
    | "A Y" -> 6 + 2
    | "B Y" -> 3 + 2
    | "C Y" -> 0 + 2
    | "A Z" -> 0 + 3
    | "B Z" -> 6 + 3
    | "C Z" -> 3 + 3
    | _ -> 0

let scorePart2 str =
    match str with
    | "A X" -> 0 + 3
    | "B X" -> 0 + 1
    | "C X" -> 0 + 2
    | "A Y" -> 3 + 1
    | "B Y" -> 3 + 2
    | "C Y" -> 3 + 3
    | "A Z" -> 6 + 2
    | "B Z" -> 6 + 3
    | "C Z" -> 6 + 1
    | _ -> 0

let silver data = data |> Array.map scorePart1 |> Array.sum
let gold data = data |> Array.map scorePart2 |> Array.sum

printfn "--- TEST RUN ---"
printfn $"Silver: {silver test}"
printfn $"Gold: {gold test}"

printfn "--- ACTUAL RUN ---"
printfn $"Silver: {silver input}"
printfn $"Gold: {gold input}"