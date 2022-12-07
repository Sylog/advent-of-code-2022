module Aoc

open lib
open System
open FSharpx

type Token =
    | Up of string
    | Down
    | Ls
    | Dir of string
    | File of string*int64

type Tree =
    | Tree of string*int64*(Tree list)

let parseLine line =
    match String.splitChar [|' '|] line with
    | [|"$"; "cd"; ".."|] ->
        Down
    | [|"$"; "cd"; name|] ->
        Up name
    | [|"$"; "ls"|] ->
        Ls
    | [|"dir"; name|] ->
        Dir name
    | [|size; name|] ->
        File (name, (Int64.Parse size))
    | _ ->
        failwith "parse error"

let grammarError =
    function
    | x :: _ ->
        failwith $"grammar error: {x}"
    | [] ->
        failwith $"grammar error: []"

let rec createTree =
    function
    | Up name :: rest -> ls name rest
    | rest -> grammarError rest
and ls name =
    function
    | Ls :: rest -> createNode name 0L rest
    | rest -> grammarError rest
and createNode name size =
    function
    | Dir _ :: rest -> createNode name size rest
    | File (_, s) :: rest -> createNode name (size + s) rest
    | Up n :: rest -> addChildren name size [] (Up n :: rest)
    | Down :: rest -> (Tree (name, size, []), rest)
    | [] -> (Tree (name, size, []), [])
    | rest -> grammarError rest
and addChildren name size children =
    function
    | Up n :: rest ->
        let (tree, rest) = createTree (Up n :: rest)
        addChildren name size (tree :: children) rest
    | Down :: rest ->
        (Tree (name, size, children), rest)
    | [] ->
        (Tree (name, size, children), [])
    | rest -> grammarError rest

let tokensToTree tokens =
    match createTree tokens with
    | (tree, []) -> tree
    | (_, rest) -> grammarError rest

let calcSizes tree =
    let rec calc result =
        function
        Tree (name, size, children) ->
            let (sum, r) = List.fold acc (0L, result) children
            (sum + size, (name, sum + size)::r)
    and acc (sum, result) tree =
        let (s, r) = calc result tree
        (sum + s, r)
    let (_, r) = calc [] tree
    r

let sumWithinLimit limit lst =
    let withinLimit (_, size) = size <= limit
    let getSize (_, size) = size
    lst
    |> List.filter withinLimit
    |> List.map getSize
    |> List.sum

let checkArgs args =
    match args with
    | [| fn |] -> fn
    | _ -> raise (System.ArgumentException("Wrong number of arguments"))

let main fn =
    lib.readFile __SOURCE_DIRECTORY__ fn
    |> Seq.toList
    |> List.map parseLine
    |> tokensToTree
    |> calcSizes

let calcFreedSpace limit lst =
    let used =
        match lst with
        | ("/", size) :: _ -> size
        | _ -> failwith "internal error"
    let needed = used - limit
    let minFitting min (_, size) =
        if size > needed && size < min then
            size
        else
            min
    lst
    |> List.fold minFitting used

let one (args: string[]) =
    checkArgs args
    |> main
    |> sumWithinLimit 100000
    |> printfn "%i"

let two (args: string[]) =
    let total =  70000000L
    let needed = 30000000L
    let limit = total - needed

    checkArgs args
    |> main
    |> calcFreedSpace limit
    |> printfn "%i"
