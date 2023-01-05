open System.Text.RegularExpressions

let test = System.IO.File.ReadAllLines "test.txt" |> Array.toList
let input = System.IO.File.ReadAllLines "input.txt" |> Array.toList

let rec partionByValue (sep: 'a) (lst: 'a list) = 
    match List.tryFindIndex(fun x -> x = sep) lst with
    | Some x -> List.splitAt x lst |> (fun (a,b) -> a::(partionByValue sep b.Tail))
    | None -> [lst]

let (|RegexMatch|_|) pattern input =
   let m = Regex.Match(input, pattern)
   if (m.Success) then 
        Some m.Groups
   else None

let stringListTransposition (lst: string list)=
    let arr = Array2D.init lst.Length lst.[0].Length (fun r c -> lst.[r].[c]) 
    let arrT = Array2D.init (arr |> Array2D.length2) (arr |> Array2D.length1) (fun r c -> arr.[c,r])
    [0..(Array2D.length1 arrT)-1] |> List.map (fun x -> arrT.[x,*] |> System.String.Concat)

let moveItems (l: string list) qsd =
    let q, s, d = qsd
    let items = l[s].[..q-1] |> Seq.rev |> System.String.Concat
    let newL = List.init (l.Length) (
        fun x -> 
            if x = s then l[s].[q..]
            elif x = d then items + l[d]
            else l[x]
        )
    newL

let moveItemsTogether (l: string list) qsd =
    let q, s, d = qsd
    let items = l[s].[..q-1]
    let newL = List.init (l.Length) (
        fun x -> 
            if x = s then l[s].[q..]
            elif x = d then items + l[d]
            else l[x]
        )
    newL

let silver data = 
    let splitData = data |> partionByValue ""
    let layout = splitData.[0] 
                 |> List.map (
                    fun x -> x 
                             |> Seq.toArray 
                             |> Seq.indexed |> Seq.filter (fun x -> ((fst x)-1)%4=0)
                             |> Seq.map snd
                             |> System.String.Concat
                             )
                 |> stringListTransposition
                 |> List.map (fun x -> x |> String.filter (fun z -> z <> ' '))
                 |> List.map (fun x -> x[0..(String.length x)-2])
    let moves = splitData.[1] |> List.map (fun x -> 
        match x with
        | RegexMatch "move (\d+) from (\d+) to (\d+)" x -> (int x.[1].Value, int x.[2].Value-1, int x.[3].Value-1)
        | _ -> failwith "invalid move present"
        )
    let finalLayout = moves |> List.fold moveItems layout
    finalLayout |> List.map(fun x -> x[0]) |> System.String.Concat

let gold data = 
    let splitData = data |> partionByValue ""
    let layout = splitData.[0] 
                 |> List.map (
                    fun x -> x 
                             |> Seq.toArray 
                             |> Seq.indexed |> Seq.filter (fun x -> ((fst x)-1)%4=0)
                             |> Seq.map snd
                             |> System.String.Concat
                             )
                 |> stringListTransposition
                 |> List.map (fun x -> x |> String.filter (fun z -> z <> ' '))
                 |> List.map (fun x -> x[0..(String.length x)-2])
    let moves = splitData.[1] |> List.map (fun x -> 
        match x with
        | RegexMatch "move (\d+) from (\d+) to (\d+)" x -> (int x.[1].Value, int x.[2].Value-1, int x.[3].Value-1)
        | _ -> failwith "invalid move present"
        )
    let finalLayout = moves |> List.fold moveItemsTogether layout
    finalLayout |> List.map(fun x -> x[0]) |> System.String.Concat
    

printfn "--- TEST RUN ---"
printfn $"Silver: {silver test}"
printfn $"Gold: {gold test}"

printfn "--- ACTUAL RUN ---"
printfn $"Silver: {silver input}"
printfn $"Gold: {gold input}"