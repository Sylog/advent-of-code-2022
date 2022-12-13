module Aoc

open lib
open System
open FSharpx

let checkArgs args =
    match args with
    | [| fn |] -> fn
    | _ -> raise (System.ArgumentException("Wrong number of arguments"))

type Token =
    | Left
    | Right
    | Comma
    | Num of int

type Tree =
    | Tree of Tree list
    | Node of int

let parseError n =
    failwith $"parse error {n}"

let parseLine1 (str: string) =
    let ZERO = int '0'
    let NINE = int '9'
    let rec findNonDigit i =
        let c = int str[i]
        if ZERO <= c && c <= NINE then
            findNonDigit (i + 1)
        else
            i
    let rec tokenize acc i size =
        if i >= size then
            List.rev acc
        else
            match str[i] with
            | '[' -> tokenize (Left :: acc) (i + 1) size
            | ']' -> tokenize (Right :: acc) (i + 1) size
            | ',' -> tokenize (Comma :: acc) (i + 1) size
            | c ->
                let j = findNonDigit i
                if j = i then parseError 1
                let n = Int32.Parse str[i..(j-1)]
                tokenize (Num n :: acc) j size

    let rec parseList acc =
        function
        | Left :: rest ->
            let (t, rest') = parseList [] rest
            parseList2 (t :: acc) rest'
        | Right :: rest ->
            ret acc rest
        | Num n :: rest ->
            parseList2 (Node n :: acc) rest
        | _ ->
            parseError 2
    and parseList2 acc =
        function
        | Right :: rest ->
            ret acc rest
        | Comma :: rest ->
            parseList acc rest
        | _ ->
            parseError 3
    and ret list tokens = (Tree (List.rev list), tokens)

    match tokenize [] 0 (String.length str) with
    | Left :: rest ->
        match parseList [] rest with
        | (t, []) -> t
        | _ -> parseError 4
    | _ -> parseError 5

let parseLine0 =
    function
    | "" -> None
    | line -> Some (parseLine1 line)

let rec pairs acc =
    function
    | None :: rest -> pairs acc rest
    | Some a :: Some b :: rest -> pairs ((a, b) :: acc) rest
    | [] -> List.rev acc
    | _ -> parseError 6

let parseFile fn =
    lib.readFile __SOURCE_DIRECTORY__ fn
    |> Seq.toList
    |> List.map parseLine0

type Order =
    | Less
    | Equal
    | Greater

let rec compare =
    function
    | (Node x, Node y) ->
        if x < y then
            Less
        else if x = y then
            Equal
        else
            Greater
    | (Node x, Tree b) -> compareList ([Node x], b)
    | (Tree a, Node y) -> compareList (a, [Node y])
    | (Tree a, Tree b) -> compareList (a, b)
and compareList =
    function
    | ([], []) -> Equal
    | ([], _) -> Less
    | (_, []) -> Greater
    | (a :: al, b :: bl) ->
        match compare (a, b) with
        | Less -> Less
        | Equal -> compareList (al, bl)
        | Greater -> Greater

let sumCorrect list =
    let addCorrect acc =
        function
        | n, Less -> acc + n + 1
        | n, Equal -> acc + n + 1
        | _, Greater -> acc

    list
    |> pairs []
    |> List.map compare
    |> List.indexed
    |> List.fold addCorrect 0

type Packet =
    | Content
    | Divider

let calcDecoderKey list =
    let divider1 = parseLine1 "[[2]]"
    let divider2 = parseLine1 "[[6]]"

    let comparer (a, _) (b, _) =
        match compare (a, b) with
        | Less -> -1
        | Equal -> 0
        | Greater -> 1

    let getDiderIndex x =
        match x with
        | (i, (_, Divider)) -> Some (i + 1)
        | _ -> None

    let list' =
        list
        |> List.choose id
        |> List.map (fun p -> (p, Content))

    (divider1, Divider) :: (divider2, Divider) :: list'
    |> List.sortWith comparer
    |> List.indexed
    |> List.choose getDiderIndex
    |> function
        | [a; b] -> a * b
        | _ -> failwith "internal error" 

let one (args: string[]) =
    checkArgs args
    |> parseFile
    |> sumCorrect
    |> printfn "%A"

let two (args: string[]) =
    checkArgs args
    |> parseFile
    |> calcDecoderKey
    |> printfn "%A"
