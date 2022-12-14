module Aoc

open lib
open System
open FSharpx

type Operation =
    | Mult of int64
    | Add of int64
    | Square

type Monkey = {
    id: int
    opreation: Operation
    testDivisor: int64
    trueMonkey: int
    falseMonkey: int
    itemsInspected: int64
}

module Queue =
    let create n =
        Array.create n []

    let add a i v =
        Array.set a i (v :: a[i])

    let flush (a: (int64 list)[]) i =
        let backwords = a[i]
        Array.set a i []
        List.rev backwords

let rec parseLine =
    let rec getItems acc =
        function
        | [] -> List.rev acc
        | "" :: item :: rest -> getItems (item :: acc) rest
        | _ ->
            failwith "parse error"
    function
    | ["Monkey"; id; ""] -> ["Monkey"; id]
    | "" :: "" :: "Starting" :: "items" :: rest -> getItems [] rest
    | [""; ""; "Operation"; ""; "new"; "="; "old"; op; arg] -> ["Operation"; op; arg]
    | [""; ""; "Test"; ""; "divisible"; "by"; divisor] -> ["divisible"; divisor]
    | [""; ""; ""; ""; "If"; "true"; ""; "throw"; "to"; "monkey"; id] -> ["true"; id]
    | [""; ""; ""; ""; "If"; "false"; ""; "throw"; "to"; "monkey"; id] -> ["false"; id]
    | tokens -> failwith $"parse error ({tokens})"

let createMonkey queues lines =
    let tokenize str = 
        String.splitChar [|' '; ','; ':'|] str
        |> Array.toList

    let createOp =
        function
        | ("+", arg) -> Int64.Parse arg |> Add
        | ("*", "old") -> Square
        | ("*", arg) -> Int64.Parse arg |> Mult
        | _ -> failwith "operation parse error"

    let addItems index items =
        List.iter (Queue.add queues index) items

    let create =
        function
        | [
            ["Monkey"; id];
            items;
            ["Operation"; op; arg];
            ["divisible"; divisor];
            ["true"; trueMonkey];
            ["false"; falseMonkey]
            ] ->
            let i = Int32.Parse id
            items
            |> List.map Int64.Parse
            |> addItems i
            {
                id = i
                opreation = createOp (op, arg)
                testDivisor = Int64.Parse divisor
                trueMonkey = Int32.Parse trueMonkey
                falseMonkey = Int32.Parse falseMonkey
                itemsInspected = 0
            }
        | tokens -> failwith $"parse error ({tokens})"

    lines
    |> List.map tokenize
    |> List.map parseLine
    |> create

let parse lines =
    let rec monkeys m ms =
        function
        | [] ->
             List.rev ((List.rev m) :: ms)
        | "" :: rest ->
            monkeys [] ((List.rev m) :: ms) rest
        | x :: rest ->
            monkeys (x :: m) ms rest

    let ms =
        lines
        |> monkeys [] []
    let queues =
        List.length ms 
        |> Queue.create

    (queues, List.map (createMonkey queues) ms)

let playRound (factor, bigDivisor, queues, monkeys) =
    let operate arg1 =
        let arg1' = arg1 % bigDivisor
        function
        | Add arg2 -> arg1' + arg2
        | Mult arg2 -> arg1' * arg2
        | Square -> arg1' * arg1'

    let inspect (monkey: Monkey) item =
        let a = operate item monkey.opreation
        let b = a / factor
        if (b % monkey.testDivisor) = 0 then
            Queue.add queues monkey.trueMonkey b
        else
            Queue.add queues monkey.falseMonkey b

    let playTurn monkey =
        let items = Queue.flush queues monkey.id
        let n: int64 = List.length items
        List.iter (inspect monkey) items        
        { monkey with itemsInspected = monkey.itemsInspected + n }

    let monkeys' =
        monkeys
        |> List.map playTurn
    
    (factor, bigDivisor, queues, monkeys')

let rec playRounds n state =
    let calc =
        function
        | a :: b :: _ ->
            a * b
        | _ -> failwith "too few monkeys"

    if n > 0 then
        state
        |> playRound
        |> playRounds (n - 1)
    else
        let (_, _, _, ms) = state
        List.map (fun m -> m.itemsInspected) ms
        |> List.sort
        |> List.rev
        |> calc

let checkArgs args =
    match args with
    | [| fn |] -> fn
    | _ -> raise (System.ArgumentException("Wrong number of arguments"))

let readFile fn =
    lib.readFile __SOURCE_DIRECTORY__ fn
    |> Seq.toList
    |> parse

let playGame factor rounds (queues, monkeys) =
    let bigDivisor =
        monkeys
        |> List.fold (fun acc m -> acc * m.testDivisor) 1L
    (factor, bigDivisor, queues, monkeys)
    |> playRounds rounds

let one args =
    checkArgs args
    |> readFile
    |> playGame 3L 20
    |> printfn "%i"

let two args =
    checkArgs args
    |> readFile
    |> playGame 1L 10000
    |> printfn "%i"
