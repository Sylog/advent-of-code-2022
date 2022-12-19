module Aoc

open lib
open System
open FSharpx

type Material =
    | Air
    | Stone
    | Sand

// type Matrix = {
//         xsize: int
//         ysize: int
//         matrix: array<Material>
//     }
//     with static member create xs ys = {
//             xsize = xs
//             ysize = ys
//             matrix = Array.create (xs * ys) Air
//          }
//          member this.index i j = j * this.xsize + i
//          member this.set i j c =
//                 Array.set this.matrix (this.index i j) c
//          member this.get i j =
//                 this.matrix[this.index i j]
//             member this.printSection (x1, y1) (x2, y2) =
//                 for j in y1..y2 do
//                     for i in x1..x2 do
//                         match this.matrix[this.index i j] with
//                         | Air -> printf "."
//                         | Stone -> printf "#"
//                         | Sand -> printf "o"
//                     printfn ""

let parseLine line =
    let arr = String.splitChar [|','; '='; ':'|] line
    [arr[1]; arr[3]; arr[5]; arr[7]]
    |> List.map Int32.Parse


let checkArgs args =
    match args with
    | [| fn |] -> fn
    | _ -> raise (System.ArgumentException("Wrong number of arguments"))

let parseFile fn =
    lib.readFile __SOURCE_DIRECTORY__ fn
    |> Seq.toList
    |> List.map parseLine

let rowNoBeacon row sensors =
    let createSet0 n x =
        {(x - n) .. (x + n)}
        |> Set.ofSeq

    let createSet1 x (beaconDist, rowDist, beaconSet) =
        let d = beaconDist - rowDist
        if d < 0 then
             Set.empty
        else
            let set = createSet0 d x
            Set.difference set beaconSet

    let createSet =
        function
        | [x1; y1; x2; y2] ->
            let beaconSet =
                if y2 = row then Set.empty.Add(x2)
                else Set.empty
            ((abs (x2 - x1)) + (abs (y2 - y1)), abs (row - y1), beaconSet)
            |> createSet1 x1
        | _ -> failwith "internal error"
    
    sensors
    |> List.map createSet
    |> Set.unionMany


let one args =
    checkArgs args
    |> parseFile
//    |> rowNoBeacon 10
    |> rowNoBeacon 2000000
    |> Set.count
    |> printfn "%A"

let calcFrequency (i, j) =
    let f: int64 = 4000000
    (int64 i) * f + (int64 j)

let two = one