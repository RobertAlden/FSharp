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
let maxRowProduct (r: int64 array2d) = [for i in [0..((r.GetLength 1)-1)] -> r[i, *] |> Array.reduce(*)]
                                       |> List.max
let getDiag (r: int64 array2d) = List.zip [0..((r.GetLength 0)-1)] [0..((r.GetLength 1)-1)] 
                                 |> List.map (fun (x,y) -> r[x,y]) 
                                 |> List.reduce(*)
let maxProduct (arr: int64 array2d) = 
    List.max [maxRowProduct arr;
              arr |> transpose |> maxRowProduct; 
              getDiag arr; 
              arr |> reverse |> getDiag;]

let answer =  List.max [for window in (windowed2D 4 4 grid) -> maxProduct window]
printfn $"Answer: {answer}"