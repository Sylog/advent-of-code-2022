module Aoc

open lib
open System
open FSharpx

type Matrix = {
        xsize: int
        ysize: int
        matrix: array<int>
    }
    with static member create xs ys = {
            xsize = xs
            ysize = ys
            matrix = Array.create (xs * ys) 0
         }
         member this.index i j = j * this.xsize + i
         member this.set i j c =
                Array.set this.matrix (this.index i j) c
         member this.get i j =
                this.matrix[this.index i j]
         member this.inc i j =
            let index = this.index i j
            let newval = this.matrix[index] + 1
            Array.set this.matrix index newval
         member this.fold f acc =
            Array.fold f acc this.matrix
         member this.print () =
            for j in 0..(this.ysize - 1) do
                for i in 0..(this.xsize - 1) do
                    printf "%i" this.matrix[this.index i j]
                printfn ""

let zero = int '0'
let parseRow (j, (m: Matrix)) (line: string) =
    let parseColumn i c =
        m.set i j ((int c) - zero)
        |> ignore

    line            
    |> String.iteri parseColumn
    (j + 1, m)

let parseMatrix lines =
    let maxy = List.length lines
    let maxx = String.length (List.head lines)
    let m = Matrix.create maxx maxy
    lines
    |> List.fold parseRow (0, m)
    |> ignore
    m

let createMatrix fn =
    let lines = lib.readFile __SOURCE_DIRECTORY__ fn
    lines
    |> Seq.toList
    |> parseMatrix

let seenMatrix (m: Matrix) =
    let size = m.xsize
    assert (size = m.ysize)
    let sm = Matrix.create size size
    let up = seq { 0 .. (size - 1) }
    let down = seq { (size - 1) .. -1 .. 0 }
    let check cellrange rowrange =
        for j in rowrange do
            let mutable leftmax = -1
            let mutable rightmax = -1
            for i in cellrange do
                let leftcurrent = m.get i j
                let rightcurrent = m.get j i
                if leftcurrent > leftmax then
                    leftmax <- leftcurrent
                    sm.inc i j
                if rightcurrent > rightmax then
                    rightmax <- rightcurrent
                    sm.inc j i
    check up up
    check down up
    sm

let calcViewingDistances m =
    let size = m.xsize
    assert (size = m.ysize)
    let vdm = Matrix.create size size
    let maybeContinue i j max n limitf continuef =
        if limitf () then
            n
        else
            let current = m.get i j
            if current < max then
                (continuef ()) max (n + 1)
            else
                n + 1
    let rec checkLeft i j max n =
        maybeContinue i j max n (fun () -> i < 0) (fun () -> checkLeft (i - 1) j)
    let rec checkRight i j  max n =
        maybeContinue i j max n (fun () -> i >= size) (fun () -> checkRight (i + 1) j)
    let rec checkUp i j  max n =
        maybeContinue i j max n (fun () -> j < 0) (fun () -> checkUp i (j - 1))
    let rec checkDown i j max n =
        maybeContinue i j max n (fun () -> j >= size) (fun () -> checkDown i (j + 1))
    for j in 0 .. (size - 1) do
        for i in 0 .. (size - 1) do
            let current = m.get i j
            let l = checkLeft (i - 1) j current 0
            let r = checkRight (i + 1) j current 0
            let u = checkUp i (j - 1) current 0
            let d = checkDown i (j + 1) current 0
            let vd = l * r * u * d
            vdm.set i j vd
    vdm
    
let countVisable (m: Matrix) =
    let count acc hits =
        if hits > 0 then
            acc + 1
        else
            acc
    m.fold count 0

let findMax (m: Matrix) =
    let max a b =
        if a > b then a else b
    m.fold max 0

let checkArgs args =
    match args with
    | [| fn |] -> fn
    | _ -> raise (System.ArgumentException("Wrong number of arguments"))

let one args =
    checkArgs args
    |> createMatrix
    |> seenMatrix
    |> countVisable
    |> printfn "%A"

let two args =
    checkArgs args
    |> createMatrix
    |> calcViewingDistances
    |> findMax
    |> printfn "%A"
//    |> fun m -> m.print ()
