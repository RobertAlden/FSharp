(*
If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?


NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) 
contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
*)

let resolveOnes n = 
    match n with 
    | "0" -> ""
    | "1" -> "one"
    | "2" -> "two"
    | "3" -> "three"
    | "4" -> "four"
    | "5" -> "five"
    | "6" -> "six"
    | "7" -> "seven"
    | "8" -> "eight"
    | "9" -> "nine"
    | _ -> ""

let resolveTens n =
    match n with
    | ["0"; ones] -> "" + resolveOnes ones
    | ["1"; "0"] -> "ten"
    | ["1"; "1"] -> "eleven"
    | ["1"; "2"] -> "twelve"
    | ["1"; "3"] -> "thirteen"
    | ["1"; "4"] -> "fourteen"
    | ["1"; "5"] -> "fifteen"
    | ["1"; "6"] -> "sixteen"
    | ["1"; "7"] -> "seventeen"
    | ["1"; "8"] -> "eighteen"
    | ["1"; "9"] -> "nineteen"
    | ["2"; ones] -> "twenty" + resolveOnes ones
    | ["3"; ones] -> "thirty" + resolveOnes ones
    | ["4"; ones] -> "forty" + resolveOnes ones
    | ["5"; ones] -> "fifty" + resolveOnes ones
    | ["6"; ones] -> "sixty" + resolveOnes ones
    | ["7"; ones] -> "seventy" + resolveOnes ones
    | ["8"; ones] -> "eighty" + resolveOnes ones
    | ["9"; ones] -> "ninety" + resolveOnes ones
    | _ -> ""

let convertToText n = 
    match n with
    | [ones] -> resolveOnes ones
    | [tens; ones] -> resolveTens [tens; ones]
    | ["0"; tens; ones] -> "" + resolveTens [tens; ones]
    | [x; "0"; "0"] -> resolveOnes x + "hundred"
    | [x; tens; ones] -> resolveOnes x + "hundredand" + resolveTens [tens; ones]
    | ["1";"0";"0";"0"] -> "onethousand"
    | _ -> ""

let digitsOf n = 
    string n |> Seq.toList |> List.map(fun x -> string x)

let answer = [1..1000] |> List.map(fun x -> convertToText (digitsOf x)) |> List.reduce(+) |> String.length
printfn $"Answer: {answer}"
    