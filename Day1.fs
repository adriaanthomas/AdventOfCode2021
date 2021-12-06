module AdventOfCode2021.Day1

open Utilities

let readings =
    readLines "day1.txt"
    |> List.map int

let countDepthIncreases data =
    data
    |> List.pairwise
    |> List.filter (fun (d1, d2) -> d2 > d1)
    |> List.length

let countDepthIncreasesWithNoiseReduction data =
    data
    |> List.windowed 3
    |> List.map List.sum
    |> countDepthIncreases

let run() =
    printfn $"Number of depth increases: {countDepthIncreases readings}"
    printfn $"Number of depth increases with noise reduction: {countDepthIncreasesWithNoiseReduction readings}"
