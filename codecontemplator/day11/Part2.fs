// Learn more about F# at http://fsharp.org

open System

type Monkey = { 
    mutable items : int64 list
    op : int64 -> int64
    test : int64*int64*int64
    mutable inspections : int
} 

let monkeysSample : Monkey array = [|
    { 
      items = [ 79L;  98L ]
      op = fun x -> x * 19L
      test = (23L, 2L, 3L)
      inspections = 0
    };
    {
        items = [ 54L; 65L; 75L; 74L ]
        op = fun x -> x + 6L
        test = (19L, 2L, 0L)
        inspections = 0
    };
    {
        items = [ 79L; 60L; 97L ]
        op = fun x -> x * x
        test = (13L, 1L, 3L)
        inspections = 0
    };
    {
        items = [ 74L ]
        op = fun x -> x + 3L
        test = (17L, 0L, 1L)
        inspections = 0
    };
|]


let monkeysInput : Monkey array = [|
    { 
      items = [ 93L; 54L; 69L; 66L; 71L ]
      op = fun x -> x * 3L
      test = (7L, 7L, 1L)
      inspections = 0
    };
    { 
      items = [ 89L; 51L; 80L; 66L ]
      op = fun x -> x * 17L
      test = (19L, 5L, 7L)
      inspections = 0
    };
    { 
      items = [ 90L; 92L; 63L; 91L; 96L; 63L; 64L ]
      op = fun x -> x + 1L
      test = (13L, 4L, 3L)
      inspections = 0
    };
    { 
      items = [ 65L; 77L ]
      op = fun x -> x + 2L
      test = (3L, 4L, 6L)
      inspections = 0
    };
    { 
      items = [ 76L; 68L; 94L ]
      op = fun x -> x * x
      test = (2L, 0L, 6L)
      inspections = 0
    };
    { 
      items = [ 86L; 65L; 66L; 97L; 73L; 83L ]
      op = fun x -> x + 8L
      test = (11L, 2L, 3L)
      inspections = 0
    };
    { 
      items = [ 78L ]
      op = fun x -> x + 6L
      test = (17L, 0L, 1L)
      inspections = 0
    };
    { 
      items = [ 89L; 57L; 59L; 61L; 87L; 55L; 55L; 88L ]
      op = fun x -> x + 7L
      test = (5L, 2L, 5L)
      inspections = 0
    };
|]

let monkeys = monkeysInput
let rounds = 10000

let modulo = 
    monkeys |> 
    Array.map (fun m -> match m.test with (d,_,_) -> d) |>
    Array.fold (*) 1L

for round in 1..rounds do
    for index in 0..(Array.length monkeys - 1) do
        let monkey = monkeys.[index]
        let items = monkey.items
        monkey.items <- []
        monkey.inspections <- monkey.inspections + (List.length items)
        for item in items do
            let worryLevel : int64 = monkey.op item
            //let worryLevel2 : int = worryLevel / 3
            let worryLevel2 = worryLevel % modulo
            let (divisor, ma, mb) = monkey.test
            let targetIndex = 
                if worryLevel2 % divisor = 0L then
                    ma
                else
                    mb
            let monkeyTarget = monkeys.[int targetIndex]
            //printfn "Monkey %d throws (%d -> %d -> %d) to monkey %d" index item worryLevel worryLevel2 targetIndex
            monkeyTarget.items <- worryLevel2 :: monkeyTarget.items 
    printfn "Round %d\n" round
    for index in 0..(Array.length monkeys - 1) do
        printfn "Monkey %d: %A\n" index (monkeys.[index].inspections)

let result =
    let [| a; b |] = 
        monkeys |> 
        Array.map (fun x -> x.inspections) |>
        Array.sortDescending |>
        Array.take 2
    (bigint a) * (bigint b)

[<EntryPoint>]
let main argv =
    printfn "%A\n" result
    0 // return an integer exit code
