open System.Text.RegularExpressions

let test = System.IO.File.ReadAllLines "test.txt" |> Array.toList
//let input = System.IO.File.ReadAllLines "input.txt" |> Array.toList



type directory = {
    size : int
    dirs : directory list
}

let rec dirsize dir = 
    match dir.dirs with
    | [] -> dir.size
    | _ -> dir.dirs |> List.map dirsize 
                    |> List.reduce(+) 
                    |> (+) dir.size

let (|RegexMatch|_|) pattern input =
   let m = Regex.Match(input, pattern)
   if (m.Success) then 
        Some m.Groups
   else None

let parseFile line =
    match line with
    | RegexMatch "\$ cd (.+)" line -> "cd"
    | RegexMatch "\$ ls" line -> "ls"
    | RegexMatch "dir (.+)" line -> "dir"
    | RegexMatch "(\d+)" line -> "filesize"
    | _ -> failwith $"Invalid statement: {line}" 

let silver data = data |> List.map parseFile
                  
let gold data = data

printfn "--- TEST RUN ---"
printfn $"Silver: {silver test}"
printfn $"Gold: {gold test}"