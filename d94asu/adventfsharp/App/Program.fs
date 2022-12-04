open Aoc
open System

[<EntryPoint>]
let main args =
    printfn "Part : %A" args[0]
    if args[0] = "1" then
        Aoc.one args[1..]
    elif args[0] = "2" then
        Aoc.two args[1..]
    else
        raise (System.ArgumentException("Wrong arguments"))
    |> ignore
    0
