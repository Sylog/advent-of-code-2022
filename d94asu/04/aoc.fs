module Aoc

open lib
open System
open FSharpx

let parseInterval str =
    str
    |> String.splitChar [|'-'; ','|]
    |> Array.map Int32.Parse
    |> function
        | [|a; b; c; d|] -> ((a, b), (c, d))
        | _ -> failwith "parse error"

let checkArgs args =
    match args with
    | [| fn |] -> fn
    | _ -> raise (System.ArgumentException("Wrong number of arguments"))

let checkContains (x, y)  =
    let check (a, b) (c, d) =
        (a <= c) && (b >= d)
    (check x y) || (check y x)

let checkOverlap ((a, b), (c, d))  =
    (b >= c) && (a <= d)

let main predicate fn =
    lib.readFile __SOURCE_DIRECTORY__ fn
    |> Seq.toList
    |> List.map parseInterval
    |> List.filter predicate
    |> List.length

let one (args: string[]) =
    checkArgs args
    |> main checkContains
    |> printfn "%A"

let two (args: string[]) =
    checkArgs args
    |> main checkOverlap
    |> printfn "%A"
