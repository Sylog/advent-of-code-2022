// Learn more about F# at http://fsharp.org

open System

type Monkey = { 
    mutable items : int list
    op : int -> int
    test : int*int*int
    mutable inspections : int
} 

let monkeysSample : Monkey array = [|
    { 
      items = [ 79; 98 ]
      op = fun x -> x * 19
      test = (23, 2, 3)
      inspections = 0
    };
    {
        items = [ 54; 65; 75; 74 ]
        op = fun x -> x + 6
        test = (19, 2, 0)
        inspections = 0
    };
    {
        items = [ 79; 60; 97 ]
        op = fun x -> x * x
        test = (13, 1, 3)
        inspections = 0
    };
    {
        items = [ 74 ]
        op = fun x -> x + 3
        test = (17, 0, 1)
        inspections = 0
    };
|]

let monkeysInput : Monkey array = [|
    { 
      items = [ 93; 54; 69; 66; 71 ]
      op = fun x -> x * 3
      test = (7, 7, 1)
      inspections = 0
    };
    { 
      items = [ 89; 51; 80; 66 ]
      op = fun x -> x * 17
      test = (19, 5, 7)
      inspections = 0
    };
    { 
      items = [ 90; 92; 63; 91; 96; 63; 64 ]
      op = fun x -> x + 1
      test = (13, 4, 3)
      inspections = 0
    };
    { 
      items = [ 65; 77 ]
      op = fun x -> x + 2
      test = (3, 4, 6)
      inspections = 0
    };
    { 
      items = [ 76; 68; 94 ]
      op = fun x -> x * x
      test = (2, 0, 6)
      inspections = 0
    };
    { 
      items = [ 86; 65; 66; 97; 73; 83 ]
      op = fun x -> x + 8
      test = (11, 2, 3)
      inspections = 0
    };
    { 
      items = [ 78 ]
      op = fun x -> x + 6
      test = (17, 0, 1)
      inspections = 0
    };
    { 
      items = [ 89; 57; 59; 61; 87; 55; 55; 88 ]
      op = fun x -> x + 7
      test = (5, 2, 5)
      inspections = 0
    };
|]


let monkeys = monkeysInput
let rounds = 20

for round in 1..rounds do
    for index in 0..(Array.length monkeys - 1) do
        //let index = (round - 1) % (Array.length monkeys)
        let monkey = monkeys.[index]
        let items = monkey.items
        monkey.items <- []
        monkey.inspections <- monkey.inspections + (List.length items)
        for item in items do
            let worryLevel : int = monkey.op item
            let worryLevel2 : int = worryLevel / 3
            let (divisor, ma, mb) = monkey.test
            let targetIndex = 
                if worryLevel2 % divisor = 0 then
                    ma
                else
                    mb
            let monkeyTarget = monkeys.[targetIndex]
            printfn "Monkey %d throws (%d -> %d -> %d) to monkey %d" index item worryLevel worryLevel2 targetIndex
            monkeyTarget.items <- worryLevel2 :: monkeyTarget.items 
    printfn "Round %d\n" round
    for index in 0..(Array.length monkeys - 1) do
        printfn "Monkey %d: %A\n" index (monkeys.[index].items)

let result =
    let [| a; b |] = 
        monkeys |> 
        Array.map (fun x -> x.inspections) |>
        Array.sortDescending |>
        Array.take 2
    a * b

[<EntryPoint>]
let main argv =
    printfn "%d\n" result
    0 // return an integer exit code
