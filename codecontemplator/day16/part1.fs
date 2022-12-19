open System.Text.RegularExpressions
open System.IO
open System


// https://www.programmingalgorithms.com/algorithm/floyd%E2%80%93warshall-algorithm/

let A = array2D [[1;2;3];[4;5;6];[7;8;9]]

let floydWarshall (graph : int array2d) =
    let N = Array2D.length1 graph
    let distance = Array2D.copy graph
    for k in 0..(N-1) do
        for i in 0..(N-1) do
            for j in 0..(N-1) do
                if distance.[i, k] + distance.[k, j] < distance.[i, j] then
                    distance.[i, j] <- distance.[i, k] + distance.[k, j]
    distance

let INF = 99999

type Node = {
    name : string
    flow : int
    neighbors : string array
    index : int
}

let parse (s : string) : Node =
    let matches = Regex.Match(s, "^Valve ([A-Z]+) has flow rate=(\d+); tunnels? leads? to valves? (.+)$")
    { name = matches.Groups[1].Value
      flow = int matches.Groups[2].Value
      neighbors = matches.Groups[3].Value.Split(", ")
      index = -1 }


let buildGraph (fileName : string) =
    let data = File.ReadAllLines(fileName) |> Array.map parse
    let (numNodes, indexToNodeMap, nameToNodeMap) = 
        Seq.fold (fun (index, indexToNodeMap, nameToNodeMap) node -> 
                    let node' = { node with index = index }
                    let indexToNodeMap' = Map.add index node' indexToNodeMap
                    let nameToNodeMap' = Map.add node.name node' nameToNodeMap
                    (index+1,indexToNodeMap',nameToNodeMap')) 
            (0,Map.empty,Map.empty)
            data
    let fullGraph = 
        Array2D.init numNodes numNodes (fun i j -> 
            let source = indexToNodeMap.[i]
            let dest = indexToNodeMap.[j]
            if Array.contains dest.name source.neighbors then
                1
            else
                INF
        );
    let distance = floydWarshall fullGraph

    //let numReducesNodes = nodesWithlow.Length

    //let reducedGraph = 
    //    Array2D.init numReducesNodes numReducesNodes (fun i j ->
    //        let source = nodesWithFlow.[i]
    //        let dest = nodesWithFlow.[j]
    //        distance[source, dest]
    //    )

    //let reducedNodes = 
    //    [ 0..numReducesNodes-1 ] |> 
    //    List.fold (fun result i -> 
    //        let nodeIndex = nodesWithFlow.[i]
    //        Map.add i (nodes.[nodeIndex]) result) 
    //        Map.empty

    ////reducedGraph, reducedNodes 

    let rec solve (current : Node) (nodes : Node Set) (accFlow : int) (time : int) (maxResult : int) (path : (string*int*int*int) list) =

        if time <= 0 then
            //printfn "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!%A" time
            accFlow
        else
            let time' = if current.flow > 0 then time - 1 else time
            let deltaFlow = current.flow * time'
            let accFlow' = accFlow + deltaFlow
            let path' = (current.name,30-time', current.flow, deltaFlow) :: path
            //printfn "current: %A" (current.name, accFlow, time, maxResult)
            if time' = 0 || Set.isEmpty nodes then
                if (accFlow' > maxResult) then
                    printfn "solution found: %d, %A" accFlow' (List.rev path)
                accFlow'
            else

                let nodesList = Set.toList nodes

                let maxPotential =
                    Seq.zip ([time'-1..(-2)..0]) (List.sortByDescending (fun node -> node.flow) nodesList) |>
                    Seq.map (fun (t,n) -> t*n.flow) |>
                    Seq.sum

                if maxPotential + accFlow' < maxResult then
                    //printfn "pruned since %d + %d < %d" maxPotential accFlow maxResult
                    -1  // cut of this suboptimal branch
                else
                    let nodesOrderedByPotential =
                        List.sortByDescending 
                            (fun nodeDest -> 
                                let dist = distance.[current.index, nodeDest.index]
                                (time - dist) * nodeDest.flow)
                            nodesList
                
                    let mutable bestResult = maxResult
                    for nextNode in nodesOrderedByPotential do
                        let d = distance.[current.index,nextNode.index]
                        let nodes' = Set.remove nextNode nodes
                        let result = solve nextNode nodes' accFlow' (time'-d) bestResult path'
                        if result > bestResult then                        
                            //printfn "New best found: %d" result
                            bestResult <- result

                    bestResult

    let nodesWithFlow = 
        nameToNodeMap |> 
        Map.toList |> 
        List.filter (fun (_,node) -> node.flow > 0) |>
        List.map snd |>
        Set.ofList

    solve (nameToNodeMap.["AA"]) nodesWithFlow 0 30 -9999 []

printfn "%A\n" <| (buildGraph "input.txt")  // 1986
