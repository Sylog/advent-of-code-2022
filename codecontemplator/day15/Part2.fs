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
        //if by = y then
        //    if sx-diff = bx then
        //        Some (bx+1,sx+diff)
        //    else
        //        Some (sx-diff,bx-1)
        //else
            Some (sx-diff,sx+diff)

let safePositionsAll dt y = 
    Array.map (fun (s,b) -> safePositions s b y) dt |> 
    Array.map Option.toList |>
    List.concat

let getOccupied (minr,maxr) ivs =
    let ivs = List.sortBy fst ivs
    let mutable result : List<int> = []
    let mutable xc : int = minr
    for (xs,xe) in ivs do
        for x in xc..xs-1 do
            result <- x::result
        let tmp = Math.Max(xe+1,xc)
        //printfn "%d -> %d\n" xc tmp
        xc <- tmp
    for x in xc..maxr do
        result <- x::result
    result

let findPos (minr,maxr) data =
    [
        for y in 11..maxr do
            let ivs = safePositionsAll data y |> List.sortBy fst
            //printfn "y = %d\n" y
            //printfn "ivs: %A\n" ivs
            let occs = getOccupied (minr, maxr) ivs
            //printf "occ: %A\n" occs
            for occ in occs do
                yield (occ,y)
    ]

let plot  (minr,maxr) data =
    for yc in minr..maxr do
        printfn "%d : " yc
        for xc in minr..maxr do
            let ivs = safePositionsAll data yc
            let m = List.exists (fun (xmin,xmax) -> xmin <= xc && xc <= xmax) ivs
            if m then
                printf "#" 
            else
                printf "."
        printf "\n"

[<EntryPoint>]
let main argv =
    let data = parse "input.txt"
    //plot (-5,26) data
    let (x,y) = findPos (0, 4000000) data |> List.head // 10621647166538
    let res = (int64 x)*4000000L+(int64 y)
    printfn "%A" res 
    0 // return an integer exit code
