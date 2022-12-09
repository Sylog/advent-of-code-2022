module Aoc

open lib
open System
open FSharpx

let LA = int 'a'

let encodeChar c =
    let pos = (int c) - LA
    1 <<< pos

let checkArgs args =
    match args with
    | [| fn |] -> fn
    | _ -> raise (System.ArgumentException("Wrong number of arguments"))

let startOfMarker len signal =
    let limit = (String.length signal) - len
    let rec loop2 i lim state =
        if i < lim then
            let bit = encodeChar signal[i]
            if state &&& bit > 0 then
                false
            else
                loop2 (i + 1) lim (state ||| bit)
        else
            true
    let rec loop1 i =
        if i < limit then
            if loop2 i (i + len) 0 then
                i + len
            else
                loop1 (i+1)
        else
            failwith "invalid signal"
    loop1 0

let main len fn =
    lib.readFile __SOURCE_DIRECTORY__ fn
    |> Seq.toList
    |> List.map (startOfMarker len)

let one (args: string[]) =
    checkArgs args
    |> main 4
    |> printfn "%A"

let two (args: string[]) =
    checkArgs args
    |> main 14
    |> printfn "%A"
