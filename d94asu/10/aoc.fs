module Aoc

open lib
open System
open FSharpx

type Instruction =
    | Noop
    | Addx of int

type Matrix = {
        xsize: int
        ysize: int
        matrix: array<bool>
    }
    with static member create xs ys = {
            xsize = xs
            ysize = ys
            matrix = Array.create (xs * ys) false
         }
         member this.index i j = j * this.xsize + i
         member this.set i j c =
                Array.set this.matrix (this.index i j) c
         member this.rawSet i c =
                Array.set this.matrix i c
         member this.get i j =
                this.matrix[this.index i j]
         member this.print =
            for j in 0..(this.ysize - 1) do
                for i in 0..(this.xsize - 1) do
                    if this.matrix[this.index i j] then
                        printf "#"
                    else
                        printf "."
                printfn ""

let checkArgs args =
    match args with
    | [| fn |] -> fn
    | _ -> raise (System.ArgumentException("Wrong number of arguments"))

let parseLine line =
    match String.splitChar [|' '|] line with
    | [|"noop"|] -> Noop
    | [|"addx"; n|] -> Addx (Int32.Parse n)
    | _ ->
        failwith "parse error"

let queue list last = List.concat [list; [last]]

let rec addNoops program n =
    if n > 0 then
        addNoops (Noop :: program) (n - 1)
    else
        program

let alu clock operations  =
    function
    | Noop :: rest ->
            (operations, rest)
    | Addx n :: rest ->
            (queue operations (clock + 2, n), (addNoops rest 1))
    | [] ->
        failwith "premature program ending"

let findSignalStrengths times program =
    let rec loop clock x operations acc times program =
        let (|Now|NotNow|) input = if input = clock then Now else NotNow
        let (x', operations') =
            match operations with
            | (Now, n) :: rest -> (x + n, rest)
            | (NotNow, _) :: _ -> (x, operations)
            | [] -> (x, [])

        match times with
        | [] ->
            List.rev acc
        | Now :: times' ->
            loop clock x operations (clock * x :: acc) times' program
        | NotNow :: _ ->
            let (operations'', program') = alu clock operations' program
            loop (clock + 1) x' operations'' acc times program'

    loop 0 1 [] [] times program

let doDisplay program =
    let width = 40
    let hight = 6
    let display = Matrix.create 40 6    
    let rec loop clock (x: int) operations program =
        let pos = clock % width
        let (|Now|NotNow|) input = if input = clock then Now else NotNow
        let (x', operations') =
            match operations with
            | (Now, n) :: rest -> (x + n, rest)
            | (NotNow, _) :: _ -> (x, operations)
            | [] -> (x, [])
        let pixel =
            (abs (pos - x')) < 2

        display.rawSet clock pixel
        let (operations'', program') = alu clock operations' program
        if not (List.isEmpty program') then
            loop (clock + 1) x' operations'' program'
  
    loop 0 1 [] program
    display.print

let parse fn =
    lib.readFile __SOURCE_DIRECTORY__ fn
    |> Seq.toList
    |> List.map parseLine

let one (args: string[]) =
    checkArgs args
    |> parse
    |> findSignalStrengths [20; 60;  100;  140;  180; 220]
    |> List.sum
    |> printfn "%i"

let two (args: string[]) =
    checkArgs args
    |> parse
    |> doDisplay
