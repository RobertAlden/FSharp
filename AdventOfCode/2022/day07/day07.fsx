open System.Text.RegularExpressions

let test = System.IO.File.ReadAllLines "test.txt" |> Array.toList
//let input = System.IO.File.ReadAllLines "input.txt" |> Array.toList



type directory = {
    name : string
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

let parseFileIntoTree (flag:string) data =
    let node = {name=flag;size=0;dirs=[]}
    let line = data |> List.filter (fun x -> (string x).Contains ("cd "+flag)) |> List.head
    let start = data |> List.findIndex (fun x -> x = line) |> (+) 2
    let parseLine tree line =
        match line with
            | RegexMatch "\$ cd (.+)" line -> tree
            | RegexMatch "\$ ls" line -> tree
            | RegexMatch "dir (.+)" line -> {tree with dirs=tree.dirs @ [{name=line.[1].Value; size=0; dirs=[]}]}
            | RegexMatch "(\d+)" line -> {tree with size=(tree.size+(int line.[1].Value))}
            | _ -> failwith $"Invalid statement: {line}" 
    data[start..] |> List.fold parseLine node
    node
        

let silver data = 
    let tree = parseFileIntoTree data "/"
    tree
                  
let gold data = data

printfn "--- TEST RUN ---"
printfn $"Silver: {silver test}"
printfn $"Gold: {gold test}"