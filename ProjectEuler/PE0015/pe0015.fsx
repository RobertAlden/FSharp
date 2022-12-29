(*
Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down,
there are exactly 6 routes to the bottom right corner.

How many such routes are there through a 20×20 grid?
*)

let pascal (n: int64 list) = 
    match n with
    | [_] -> [1L] @ [1L]
    | _ -> [1L] @ (List.pairwise n |> List.map(fun (a,b) -> a+b)) @ [1L]

let applyN f n v =
   let g = [1..n]
        |> List.map (fun _ -> f)
        |> List.reduce (>>)
   g v

let answer = applyN pascal 40 [1L] |> (fun x -> x[List.length x/2])