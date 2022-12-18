module Aoc

open lib
open System
open FSharpx

type Space = {
        xsize: int
        ysize: int
        zsize: int
        arr: bool[,,]
    }
    with static member create xs ys zs = {
            xsize = xs
            ysize = ys
            zsize = zs
            arr = Array3D.create xs ys zs false
         }
         member this.set i j k c =
                Array3D.set this.arr i j k c
         member this.get i j k =
                this.arr[i,j,k]
         member this.map mapper = {
                xsize = this.xsize
                ysize = this.ysize
                zsize = this.zsize
                arr = Array3D.map mapper this.arr
             }
         member this.print () =
            for k in 0..(this.zsize - 1) do
                for j in 0..(this.ysize - 1) do
                    for i in 0..(this.xsize - 1) do
                        if this.arr[i,j,k] then
                            printf "#"
                        else
                            printf "."
                    printfn ""
                printfn ""
                printfn ""

let checkArgs args =
    match args with
    | [| fn |] -> fn
    | _ -> raise (System.ArgumentException("Wrong number of arguments"))

let parseLine line =
    String.splitChar [|','|] line
    |> Array.map Int32.Parse
    |> function
    | [|x; y; z|] -> (x, y, z)
    | _ -> failwith "parse error"

let parseFile fn =
    lib.readFile __SOURCE_DIRECTORY__ fn
    |> Seq.toList
    |> List.map parseLine

let createSpace cubes =
    let size = 25
//    let size = 8
    let space = Space.create size size size
    let addCube (i, j, k) =
        space.set i j k true
    List.iter addCube cubes
    space

let neighbors (i, j, k) = [
    (i - 1, j, k)
    (i + 1, j, k)
    (i, j - 1, k)
    (i, j + 1, k)
    (i, j, k - 1)
    (i, j, k + 1)
    ]

let surfaceArea0 space cubes =
    let size = space.xsize
    let insideBounds a =
        (0 <= a) && (a < size)
    let checkOccupied (i, j, k) =
        if List.forall insideBounds [i; j; k] then
            space.get i j k
        else
            false
    let countFreeFaces (i, j, k) =
        let check acc cube =
            if checkOccupied cube then
                acc - 1
            else
                acc
        List.fold check 6 (neighbors (i, j, k))
    cubes
    |> List.map countFreeFaces
    |> List.sum

let createAirInfo space =
    let size = space.xsize
    let air = Space.create size size size
    let onBoarder a =
        (a = 0) || (a = (size - 1))
    let checkNeighbor (i, j, k) =
        air.get i j k
    let checkCube (i, j, k) =
        if not (space.get i j k) then
            if not (air.get i j k) then // skip already found air
                if List.exists onBoarder [i; j; k] then
                    air.set i j k true
                elif List.exists checkNeighbor (neighbors (i, j, k)) then
                    air.set i j k true

    let checkSpace () =
            let start = 0
            let stop = size - 1
            let range = { start .. stop }
            for i in range do
                for j in range do
                    for k in range do
                        checkCube (i, j, k)

    for _ in {0 .. size} do
        checkSpace ()
    air

let surfaceArea space (air: Space) cubes =
    let size = space.xsize
    let insideBounds a =
        (0 <= a) && (a < size)
    let checkAir (i, j, k) =
        if List.forall insideBounds [i; j; k] then
            air.get i j k
        else
            true
    let countFreeFaces (i, j, k) =
        let check acc cube =
            if checkAir cube then
                acc + 1
            else
                acc
        List.fold check 0 (neighbors (i, j, k))
    cubes
    |> List.map countFreeFaces
    |> List.sum

let realSurfaceArea cubes =
    let space = createSpace cubes
    let air = createAirInfo space
    surfaceArea space air cubes

let createBadAirInfo (space: Space) =
    space.map not

let otherSurfaceArea cubes =
    let space = createSpace cubes
    let air = createBadAirInfo space
    surfaceArea space air cubes

let one (args: string[]) =
    checkArgs args
    |> parseFile
    |> otherSurfaceArea
    |> printfn "%A"

let two (args: string[]) =
    checkArgs args
    |> parseFile
    |> realSurfaceArea
    |> printfn "%A"
