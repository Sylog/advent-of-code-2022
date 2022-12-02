module Aoc

open lib
open System
open FSharpx

let checkArgs args =
    match args with
    | [| fn |] -> fn
    | _ -> raise (System.ArgumentException("Wrong number of arguments"))

// % does not handle negative number as I thought it would (added 3) 
let winnings (opponent, me) =
    (me - opponent + 3L + 1L) % 3L

let myMove (opponent, result) =
    let x = ((opponent + result - 1L) % 3L + 1L)
    if x = 0 then
        3L
    else
        x

let parseLine line =
    let parseChar =
        function
        | 'A' -> 0L
        | 'B' -> 1L
        | 'C' -> 2L
        | 'X' -> 0L
        | 'Y' -> 1L
        | 'Z' -> 2L
        | _ ->
            failwith "parseChare parse error"
    let str = String.toCharArray line
    (parseChar str[0], parseChar str[2])

let score1 (opponents, me) =
    let w = winnings (opponents, me)
    w * 3L + (me + 1L)

let score2 (opponent, result) =
    let me = myMove (opponent, result)
    result * 3L + me


let main scorefun fn =
    lib.readFile __SOURCE_DIRECTORY__ fn
    |> Seq.toList
    |> List.map (parseLine >> scorefun)
    |> List.sum

let one (args: string[]) =
    checkArgs args
    |> main score1
    |> printfn "%i"

let two (args: string[]) =
    checkArgs args
    |> main score2
    |> printfn "%i"
