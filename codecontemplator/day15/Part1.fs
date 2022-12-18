// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Text.RegularExpressions

let parse file =
    let rows = File.ReadAllLines(file)
    [|
        for row in rows do
            let m = Regex.Match(row, "Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)")
            yield (int m.Groups.[1].Value,int m.Groups.[2].Value),(int m.Groups.[3].Value,int m.Groups.[4].Value)
    |]

let mdist (sx : int, sy : int) (bx : int, by : int) = 
    Math.Abs(sx - bx) + Math.Abs(sy - by)

let safePositions (sx,sy) (bx,by) y =
    let maxd = mdist (sx,sy) (bx, by)
    let mind = mdist (sx,sy) (sx,y)
    let diff = maxd - mind
    if diff < 0 then 
        None
    else 
        if by = y then
            if sx-diff = bx then
                Some (bx+1,sx+diff)
            else
                Some (sx-diff,bx-1)
        else
            Some (sx-diff,sx+diff)

let safePositionsAll dt y = 
    Array.map (fun (s,b) -> safePositions s b y) dt |> 
    Array.map Option.toList |>
    List.concat

let sumIntervals ivs =
    let ivs = List.sortBy fst ivs
    let mutable xc : int = List.head ivs |> fst
    let mutable total = 0
    for (xs,xe) in ivs do
        let x = Math.Max(xc,xs)
        let c = xe - x + 1
        if c > 0 then
            total <- total + c
            xc <- xe + 1
    total

[<EntryPoint>]
let main argv =
    let data = parse "input.txt"
    let ivs = safePositionsAll data 2000000 
    let sum = sumIntervals ivs
    printfn "%d" sum 
    0 // return an integer exit code
