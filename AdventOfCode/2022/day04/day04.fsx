open System.Text.RegularExpressions

let test = System.IO.File.ReadAllLines "test.txt" |> Array.toList
let input = System.IO.File.ReadAllLines "input.txt" |> Array.toList

let rec partionByValue (sep: 'a) (lst: 'a list) = 
    match List.tryFindIndex(fun x -> x = sep) lst with
    | Some x -> List.splitAt x lst |> (fun (a,b) -> a::(partionByValue sep b.Tail))
    | None -> [lst]

let charListToString (cl:char list) =
    System.String.Concat(Array.ofList(cl))

type bound = {
    start_bound: int
    end_bound: int
    }

type boundPair = bound * bound

let (|IsABoundPair|_|) input =
   let m = Regex.Match(input,"(\d+)-(\d+),(\d+)-(\d+)")
   if (m.Success) then 
        Some ({ 
                start_bound = int(m.Groups.[1].Value); 
                end_bound = int(m.Groups.[2].Value) 
              },
              {
                start_bound = int(m.Groups.[3].Value); 
                end_bound = int(m.Groups.[4].Value)
              })
   else None

let parseRange s =
    match s with
    | IsABoundPair s -> s
    | _ -> failwith "Not a valid range"

let isFullyContained a b =
    (b.start_bound <= a.start_bound) && (a.end_bound <= b.end_bound)

let isOverlapping a b =
    (a.start_bound <= b.start_bound) && (b.start_bound <= a.end_bound) || 
    (a.start_bound <= b.end_bound) && (b.end_bound <= a.end_bound)

let testBothOrders f a b =
    f a b || f b a

let silver data = data
                  |> List.map parseRange
                  |> List.filter (fun x -> testBothOrders isFullyContained (fst x) (snd x))
                  |> List.length

let gold data = data 
                |> List.map parseRange
                |> List.filter (fun x -> testBothOrders isOverlapping (fst x) (snd x))
                |> List.length

printfn "--- TEST RUN ---"
printfn $"Silver: {silver test}"
printfn $"Gold: {gold test}"

printfn "--- ACTUAL RUN ---"
printfn $"Silver: {silver input}"
printfn $"Gold: {gold input}"
