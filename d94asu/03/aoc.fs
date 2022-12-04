module Aoc

open lib
open System
open FSharpx

let UZ = int 'Z'
let LA = int 'a'
let UA = int 'A'
let UPPERS = 26

let encodeChar c: int64 =
    let pos =
        let x = int c
        if x > UZ then
            x - LA
        else
            x - UA + UPPERS
    1L <<< pos

let getPriority (encoded: int64): int64 =
    let mutable x = encoded
    let mutable y = 0L
    while x > 0 do
        x <- x >>> 1
        y <- y + 1L
    y

let checkArgs args =
    match args with
    | [| fn |] -> fn
    | _ -> raise (System.ArgumentException("Wrong number of arguments"))

let splitInTheMiddle str =
    let half = (String.length str) / 2
    (str.[..(half-1)], str.[half..])

let inventaryCompartment (s: string) =
    let combine acc y = acc ||| (encodeChar y)
    s
    |> Seq.toList
    |> List.fold combine 0L

let inventaryBag (a, b) =
    (inventaryCompartment a, inventaryCompartment b)

let findPacket (a, b) =
    a &&& b

let findPacket2 =
    function
    | [a; b; c] ->
        a &&& b &&& c
    | _ ->
        failwith "internal"

let lineToPriority line =
    line
    |> splitInTheMiddle
    |> inventaryBag
    |> findPacket
    |> getPriority

let main1 fn =
    lib.readFile __SOURCE_DIRECTORY__ fn
    |> Seq.toList
    |> List.map lineToPriority
    |> List.sum

let rec traverse acc =
    function
    | a :: b :: c :: rest ->
        let x = 
            [a; b; c]
            |> List.map inventaryCompartment
            |> findPacket2
            |> getPriority
        traverse (x + acc) rest
    | [] ->
        acc
    | _ ->
        failwith "can not split into groups of three"

let main2 fn =
    lib.readFile __SOURCE_DIRECTORY__ fn
    |> Seq.toList
    |> traverse 0L

let one (args: string[]) =
    checkArgs args
    |> main1
    |> printfn "%i"

let two (args: string[]) =
    checkArgs args
    |> main2
    |> printfn "%i"
