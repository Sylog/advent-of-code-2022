// Learn more about F# at http://fsharp.org

open System.IO

module Part1 = 
    type RPS = R | P | S 
        with 
            static member parse (s : string) =
                match s with
                | "X" | "A" -> R
                | "Y" | "B" -> P
                | "Z" | "C" -> S


    let strategy : (RPS*RPS) array = 
        File.ReadAllLines("input.txt") |> 
            Array.map 
                (fun (s : string)-> 
                    let [|a;b|] = s.Split(' ')
                    RPS.parse a, RPS.parse b)

    let score ((a,b) : RPS*RPS) : int =
        let shapeScore = match b with | R -> 1 | P -> 2 | S -> 3
        let outcomeScore =
            match (a,b) with
            | (R,R) | (P,P) | (S,S) -> 3
            | (P,R) | (R,S) | (S,P) -> 0
            | _ -> 6
        shapeScore + outcomeScore


    let result = strategy |> Array.map score |> Array.sum


module Part2 =
    type Outcome = Win | Loose | Draw
        with 
            static member parse (s : string) =
                match s with
                | "X" -> Loose
                | "Y" -> Draw
                | "Z" -> Win

    type RPS = R | P | S 
        with 
            static member parse (s : string) =
                match s with
                | "A" -> R
                | "B" -> P
                | "C" -> S

    let getShape (opponent : RPS) (outcome : Outcome) : RPS =
        match (opponent, outcome) with
        | (R, Win) -> P
        | (P, Win) -> S
        | (S, Win) -> R
        | (R, Loose) -> S
        | (P, Loose) -> R
        | (S, Loose) -> P
        | _ -> opponent
    
    let strategy : (RPS*RPS) array = 
        File.ReadAllLines("input.txt") |> 
            Array.map 
                (fun (s : string)-> 
                    let [|a;b|] = s.Split(' ')
                    let oppenent = RPS.parse a
                    oppenent, getShape oppenent (Outcome.parse b))


    let score ((a,b) : RPS*RPS) : int =
        let shapeScore = match b with | R -> 1 | P -> 2 | S -> 3
        let outcomeScore =
            match (a,b) with
            | (R,R) | (P,P) | (S,S) -> 3
            | (P,R) | (R,S) | (S,P) -> 0
            | _ -> 6
        shapeScore + outcomeScore

    let result = strategy |> Array.map score |> Array.sum

[<EntryPoint>]
let main argv =
    printfn "part 1: %d" Part1.result
    printfn "part 2: %d" Part2.result
    0
