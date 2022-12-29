(*
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a^2 + b^2 = c^2
For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
*)

let triples limit = [for x in [2f..(limit/2f)] do
                         for y in [x..(limit/2f)] do 
                             yield [x; y; (sqrt ((x **2f) + (y **2f)))] ]

let answer = triples 1000f 
             |> List.filter (fun x -> List.sum x = 1000f) 
             |> List.head 
             |> List.reduce(*)
             |> int64

printfn $"Answer: {answer}"
