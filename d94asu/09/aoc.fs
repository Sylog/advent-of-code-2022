module Aoc

open lib
open System
open FSharpx

type Move =
    | Up of int
    | Down of int
    | Left of int
    | Right of int

let parseLine line =
    match String.splitChar [|' '|] line with
    | [|"U"; n|] -> Int32.Parse n |> Up
    | [|"D"; n|] -> Int32.Parse n |> Down
    | [|"L"; n|] -> Int32.Parse n |> Left
    | [|"R"; n|] -> Int32.Parse n |> Right
    | _ ->
        failwith "parse error"

let move (tail, head, trail) =
    let leftMove (x, y) = (x - 1, y)
    let rightMove (x, y) = (x + 1, y)
    let upMove (x, y) = (x, y - 1)
    let downMove (x, y) = (x, y + 1)
    let shouldTailMove (tx, ty) (hx, hy) =
        let xdiff = hx - tx |> abs
        let ydiff = hy - ty |> abs
        xdiff > 1 || ydiff > 1
    let rec moveloop times tail head trail f =
        if times > 0 then
            let head' = f head
            let (tail', trail') =
                if shouldTailMove tail head' then
                    (head, Set.add tail trail)
                else
                    (tail, trail)
            moveloop (times - 1) tail' head' trail' f
        else
            (tail, head, trail)

    function
    | Left n -> moveloop n tail head trail leftMove
    | Right n -> moveloop n tail head trail rightMove
    | Up n -> moveloop n tail head trail upMove
    | Down n -> moveloop n tail head trail downMove

let countTailTrail initPos moves =
    let (tail, head, moves') = initPos moves
    List.fold move ((0, 0), (0, 0), Set.empty) moves
    |> fun (tail, _, trail) -> Set.add tail trail
    |> Set.count

let init1 moves =
    ((0, 0), (0, 0), moves)

let checkArgs args =
    match args with
    | [| fn |] -> fn
    | _ -> raise (System.ArgumentException("Wrong number of arguments"))

let main initPos fn =
    lib.readFile __SOURCE_DIRECTORY__ fn
    |> Seq.toList
    |> List.map parseLine
    |> countTailTrail initPos

let one (args: string[]) =
    checkArgs args
    |> main init1
    |> printfn "%A"

let two = one
