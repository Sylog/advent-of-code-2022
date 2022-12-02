module lib

open System

let readFile path file =
    seq { yield! System.IO.File.ReadLines(path + "/" + file + ".txt") }

let readNumbers (path) (file) =
    readFile path file
    |> Seq.map Int64.Parse
    |> Seq.toList

exception ParseError of string
