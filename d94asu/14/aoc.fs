module Aoc

open lib
open System
open FSharpx

type Material =
    | Air
    | Stone
    | Sand

type Matrix = {
        xsize: int
        ysize: int
        matrix: array<Material>
    }
    with static member create xs ys = {
            xsize = xs
            ysize = ys
            matrix = Array.create (xs * ys) Air
         }
         member this.index i j = j * this.xsize + i
         member this.set i j c =
                Array.set this.matrix (this.index i j) c
         member this.get i j =
                this.matrix[this.index i j]
            member this.printSection (x1, y1) (x2, y2) =
                for j in y1..y2 do
                    for i in x1..x2 do
                        match this.matrix[this.index i j] with
                        | Air -> printf "."
                        | Stone -> printf "#"
                        | Sand -> printf "o"
                    printfn ""

let parseLine line =
    let parseCoord pair=
        match String.splitChar [|','|] pair with
        | [|a; b|] -> (Int32.Parse a, Int32.Parse b)
        | q -> failwith "parse error"
    
    line
    |> String.splitChar [|' '|]
    |> Array.filter (fun s -> s <> "->")
    |> Array.map parseCoord
    |> Array.toList

let checkArgs args =
    match args with
    | [| fn |] -> fn
    | _ -> raise (System.ArgumentException("Wrong number of arguments"))

let parseFile fn =
    let pairMax (x1, y1) (x2, y2) =
        (max x1 x2, max y1 y2)

    let rocks =
        lib.readFile __SOURCE_DIRECTORY__ fn
        |> Seq.toList
        |> List.map parseLine

    let sizes =
        rocks
        |> List.collect id
        |> List.fold pairMax (0, 0)

    let m =
        let (x, y) = sizes
        Matrix.create (x + 2) (y + 3)

    let storeLine ((i1, j1), (i2, j2)) =
        let range a b =
            if a < b then { a .. b }
            else { b .. a}

        if i1 = i2 then
            for j in range j1 j2 do
                m.set i1 j Stone
        else
            for i in range i1 i2  do
                m.set i j1 Stone

    let storeRock rock =
        rock
        |> List.pairwise
        |> List.iter storeLine

    List.iter storeRock rocks
    m

let countSand (m: Matrix) =
    let isAir x y =
        (m.get x  y) = Air
    let place x y =
        m.set x y Sand
        if (x = 0) || (x = (m.xsize - 1)) then
            m.ysize - y - 1
        else
            1
    let rec check y =
        function
        | [] -> None
        | x :: xs ->
            if (x = -1) || (x = m.xsize) then
                check y xs
            else
                match m.get x y with
                | Air -> Some (x, y)
                | _ -> check y xs

    let rec placeSand x y =
        if (y + 1 = m.ysize) || not (isAir x y) then
            0
        else
            match check (y + 1) [x; x - 1; x + 1] with
            | Some (x, y) -> placeSand x y
            | None ->
                place x y

    let rec dropSand acc =
        let n = (placeSand 500 0)
        if n = 0 then
            acc
        else
            dropSand (acc + n)

    dropSand 0

let addFloor (m: Matrix) =
    for i in 0..(m.xsize - 1) do
        m.set i (m.ysize - 1) Stone
    m

let one args =
    checkArgs args
    |> parseFile
    |> countSand
    |> printfn "%A"

let two args =
    checkArgs args
    |> parseFile
    |> addFloor
    |> countSand
    |> printfn "%A"
