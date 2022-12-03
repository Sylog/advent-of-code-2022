module Aoc

open lib
open System
open FSharpx

let checkArgs args =
    match args with
    | [| fn |] -> fn
    | _ -> raise (System.ArgumentException("Wrong number of arguments"))

let rec parse elves inventory =
    function
    | "" :: rest ->
        parse (inventory :: elves) [] rest
    | n :: rest ->
        parse elves (Int64.Parse n :: inventory) rest
    | [] ->
        inventory :: elves

let getNums fn =
    lib.readFile __SOURCE_DIRECTORY__ fn
    |> Seq.toList
    |> parse [] []

let one args =
    checkArgs args
    |> getNums
    |> (List.map List.sum)
    |> List.max
    |> printfn "%i"

let sumTopThree =
    function
    | a :: b :: c :: _-> a + b + c
    | _ -> -1L

let two args =
    checkArgs args
    |> getNums
    |> (List.map List.sum)
    |> List.sort
    |> List.rev
    |> sumTopThree
    |> printfn "%i"
