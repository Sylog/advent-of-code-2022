// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Collections.Generic

let rows = File.ReadAllLines("input.txt");

let n = rows.Length;
let m = rows.[0].Length

let matrix = Array2D.create n m 0

for row in 0..n-1 do
    for col in 0..m-1 do
        matrix.[row, col] <- int rows.[row].[col] - int '0'

let viewDist (vec : int array) (cur : int) : int = 
    let vec2 = Array.takeWhile (fun v -> v < cur) vec
    let l = Array.length vec2
    if l = Array.length vec then
        l
    else
        l + 1
          

let scoreAt (centerR, centerC) =
    let cur = matrix.[centerR,centerC]
    let row = matrix.[centerR,*]
    let left = row.[0..centerC-1] |> Array.rev
    let right = row.[centerC+1..]

    let col = matrix.[*,centerC]
    let up = col.[0..centerR-1] |> Array.rev
    let down = col.[centerR+1..]

    
    let lscore =   (viewDist left cur)
    let rscore = (viewDist right cur)
    let uscore = (viewDist up cur)
    let dscore = (viewDist down cur)

    lscore * rscore *uscore * dscore


scoreAt (1,2) |> printf "score %A\n"
scoreAt (3,2) |> printf "score %A\n"

let mutable maxScore = -1

for centerR in 0..n-1 do
    for centerC in 0..m-1 do
        let score = scoreAt (centerR, centerC)
        if score > maxScore then
            maxScore <- score

[<EntryPoint>]
let main argv =
    printf "%A" maxScore

    0 // return an integer exit code
