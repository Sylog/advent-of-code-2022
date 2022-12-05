module Aoc

open lib
open System
open FSharpx

let checkArgs args =
    match args with
    | [| fn |] -> fn
    | _ -> raise (System.ArgumentException("Wrong number of arguments"))

let rec splitFile stackStrings =
    function
    | "" :: moveStrings ->
        (stackStrings, moveStrings)
    | l :: ls ->
        splitFile (l :: stackStrings) ls
    | [] ->
        failwith "parse error"

let parseStack stackStrings =
    match stackStrings with
    | (stackNrLine :: lines) ->
        let n = (String.length stackNrLine) / 4 + 1
        let a = Array.create n []
        let rec handleChar i n (line: char array) =
            let m = 1 + i * 4
            if m < n then
                match line[m] with
                | ' ' ->
                    handleChar (i + 1) n line
                | c ->
                    a[i] <- c :: a[i]
                    handleChar (i + 1) n line
        let handleLine line =
            let n = String.length line
            handleChar 0 n (Seq.toArray line)
        List.iter handleLine lines
        a
    | [] ->
        failwith "parse error"

let parseMoves lines =
    let parse line =
        match String.splitChar [|' '|] line with
        | [|"move"; m; "from"; f; "to"; t|] ->
            (Int32.Parse m, Int32.Parse f, Int32.Parse t)
        | _ ->
            failwith "internal error"
    List.map parse lines   

let parser (stackStrings, moveStrings) =
    (parseStack stackStrings, parseMoves moveStrings)

let parseFile fn =
    lib.readFile __SOURCE_DIRECTORY__ fn
    |> Seq.toList
    |> splitFile []
    |> parser

let rec crateMover9000 n toList fromList =
    match (n, fromList) with
    | (0, _) ->
        (toList, fromList) 
    | (_, x :: xs) ->
        crateMover9000 (n - 1) (x :: toList) xs
    | _ ->
        failwith "internal error"

let crateMover9001 n toList fromList =
    let f = List.toArray fromList
    let crates = f[..(n-1)]
    let f' = Array.toList f[n..]
    let t' = List.concat [Array.toList crates; toList]
    (t', f')

let makeMoves crane ((stacks: list<char> array), moves) =
    let makeMoves (m, f, t) =
        let (tl, fl) = crane m stacks[t-1] stacks[f-1]
        stacks[t-1] <- tl
        stacks[f-1] <- fl
        ()
    List.iter makeMoves moves
    stacks

let topCrates stacks =
    let top crates =
        match crates with
        | c :: cs ->
            c.ToString ()
        | _ ->
            failwith "empty stack"
    Array.map top stacks
    |> String.concat ""

let one (args: string[]) =
    checkArgs args
    |> parseFile
    |> makeMoves crateMover9000
    |> topCrates
    |> printfn "%s"

let two (args: string[]) =
    checkArgs args
    |> parseFile
    |> makeMoves crateMover9001
    |> topCrates
    |> printfn "%s"
