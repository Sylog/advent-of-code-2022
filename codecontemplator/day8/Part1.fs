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

let visibles = new HashSet<(int*int)>()


// from left
for row in 0..n-1 do
    let rowData = matrix.[row,*]
    let mutable col = 0
    let mutable max = -1
    while col < m do
        if matrix.[row,col] > max then
            visibles.Add((row,col)) |> ignore
            max <- matrix.[row,col]
        col <- col + 1


// from right
for row in 0..n-1 do
    let rowData = matrix.[row,*]
    let mutable col = m-1
    let mutable max = -1
    while col >= 0 do
        if matrix.[row,col] > max then
            visibles.Add((row,col)) |> ignore
            max <- matrix.[row,col]
        col <- col - 1
        

//// from above
for col in 0..m-1 do
    let colData = matrix.[*,col]
    let mutable row = 0
    let mutable max = -1
    while row < n do
        if matrix.[row,col] > max then
            visibles.Add((row,col)) |> ignore
            max <- matrix.[row,col]
        row <- row + 1

// from below
for col in 0..m-1 do
    let colData = matrix.[*,col]
    let mutable row = m-1
    let mutable max = -1
    while row >= 0 do
        if matrix.[row,col] > max then
            visibles.Add((row,col)) |> ignore
            max <- matrix.[row,col]
        row <- row - 1

let visibilityMatrix = Array2D.create n m false
for (row,col) in visibles do
    visibilityMatrix.[row,col] <- true

[<EntryPoint>]
let main argv =
    //printfn "%A" matrix
    //printfn "%A" (visibles |> Seq.toList)
    printfn "%A" (visibles |> Seq.toList |> Seq.length)
    //printf "%A" visibilityMatrix

    0 // return an integer exit code
