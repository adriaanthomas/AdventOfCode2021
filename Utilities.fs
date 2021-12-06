module AdventOfCode2021.Utilities

open System.IO

let readLines filePath = File.ReadAllLines(filePath) |> List.ofArray
