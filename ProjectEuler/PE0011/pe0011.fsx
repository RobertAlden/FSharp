(*
In the 20×20 grid below, four numbers along a diagonal line have been marked in red.

<grid in file>

The product of these numbers is 26 × 63 × 78 × 14 = 1788696.

What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in the 20×20 grid?
*)

let path = "grid.txt"
let data = System.IO.File.ReadAllLines path |> Array.map (fun x -> x.Split ' ' |> Array.map int64)

let grid = Array2D.init 20 20 (fun x y -> data.[y].[x])


let windowed2D w h (arr:int64 array2d) = [for x in [0..(Array2D.length2 arr-w)] do
                                            for y in [0..(Array2D.length1 arr-h)] do
                                                 yield arr[x..x+w-1,y..y+w-1]]


let transpose (arr: int64 array2d) =  Array2D.init (arr.GetLength 1) (arr.GetLength 0) (fun x y -> arr[y,x])
let reverse (arr: int64 array2d) =  Array2D.init (arr.GetLength 0) (arr.GetLength 1) (fun x y -> arr[(arr.GetLength 0 - 1) - x,y])
let maxProduct (arr: int64 array2d) = 
    let arrT = transpose arr
    let arrR = reverse arr
    let maxHorizontalProduct = [for i in [0..((arr.GetLength 1)-1)] -> arr[i, *] |> Array.reduce(*)] 
                               |> List.max
    let maxVerticalProduct = [for i in [0..((arrT.GetLength 1)-1)] -> arrT[i, *] |> Array.reduce(*)] 
                             |> List.max
    let diagonalProduct = List.zip [0..((arr.GetLength 0)-1)] [0..((arr.GetLength 1)-1)] 
                          |> List.map (fun (x,y) -> arr[x,y]) 
                          |> List.reduce(*)
    let reverseDiagonalProduct = List.zip [0..((arrR.GetLength 0)-1)] [0..((arrR.GetLength 1)-1)] 
                                 |> List.map (fun (x,y) -> arrR[x,y]) 
                                 |> List.reduce(*)
    List.max [maxHorizontalProduct; maxVerticalProduct; diagonalProduct; reverseDiagonalProduct]


let answer =  List.max [for window in (windowed2D 4 4 grid) -> maxProduct window]
printfn $"Answer: {answer}"