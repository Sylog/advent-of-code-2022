module Aoc

open lib
open System
open FSharpx

type Operation =
    | Mult of int
    | Add of int
    | Square

type Monkey = {
    id: int
    opreation: Operation
    testDevisor: int
    trueMonkey: int
    falseMonkey: int
    itemsInspected: int
}

module Queue =
    let create n =
        Array.create n []

    let add a i v =
        Array.set a i (v :: a[i])

    let flush (a: (int list)[]) i =
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
        | ("+", arg) -> Int32.Parse arg |> Add
        | ("*", "old") -> Square
        | ("*", arg) -> Int32.Parse arg |> Mult
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
            |> List.map Int32.Parse
            |> addItems i
            {
                id = i
                opreation = createOp (op, arg)
                testDevisor = Int32.Parse divisor
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

let playRound (factor, queues, monkeys) =
    let operate arg1 =
        function
        | Add arg2 -> arg1 + arg2
        | Mult arg2 -> arg1 * arg2
        | Square -> arg1 * arg1

    let inspect (monkey: Monkey) item =
        let a = operate item monkey.opreation
        let b = a / factor
        if (b % monkey.testDevisor) = 0 then
            Queue.add queues monkey.trueMonkey b
        else
            Queue.add queues monkey.falseMonkey b

    let playTurn monkey =
        let items = Queue.flush queues monkey.id
        let n = List.length items
        List.iter (inspect monkey) items        
        { monkey with itemsInspected = monkey.itemsInspected + n }

    let monkeys' =
        monkeys
        |> List.map playTurn
    
    (factor, queues, monkeys')

let rec playRounds n state =
    let calc =
        function
        | a :: b :: _ ->
            let a64 = int64 a
            let b64 = int64 b
            a64 * b64
        | _ -> failwith "too few monkeys"

    if n > 0 then
        state
        |> playRound
        |> playRounds (n - 1)
    else
        let (_, _, ms) = state
        List.map (fun m -> m.itemsInspected) ms
        |> List.sort
        |> List.rev
        |> calc

let checkArgs args =
    match args with
    | [| fn |] -> fn
    | _ -> raise (System.ArgumentException("Wrong number of arguments"))

let main factor rounds fn =
    lib.readFile __SOURCE_DIRECTORY__ fn
    |> Seq.toList
    |> parse
    |> fun (qs, ms) -> (factor, qs, ms)
    |> playRounds rounds

let one args =
    checkArgs args
    |> main 3 20
    |> printfn "%i"

let two args =
    checkArgs args
    |> main 1 10000
    |> printfn "%i"
