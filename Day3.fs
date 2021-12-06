module AdventOfCode2021.Day3

open System
open Utilities

let testData =
    [ "00100"
      "11110"
      "10110"
      "10111"
      "10101"
      "01111"
      "00111"
      "11100"
      "10000"
      "11001"
      "00010"
      "01010" ]

let data = readLines "Day3.txt"

let splitIntoBits (line: string) =
    let testBit index data =
        let bit = 1 <<< index
        if (data &&& bit) = bit then 1 else 0

    let len = line.Length
    let data = Convert.ToInt32(line, 2)

    [ len - 1 .. -1 .. 0 ]
    |> List.map (fun x -> testBit x data)

let combineBitCounts (counts1: int list) (counts2: int list) =
    List.zip counts1 counts2
    |> List.map (fun (x, y) -> x + y)

let bitsToInt (bits: int list) =
    let rec bitsToInt acc bits =
        match bits with
        | [] -> acc
        | head :: tail -> bitsToInt (acc + (head <<< bits.Length - 1)) tail

    bitsToInt 0 bits

let gammaAndEpsilon (data: string list) =
    let len = data.Length

    let gammaBits =
        data
        |> List.map splitIntoBits
        |> List.reduce combineBitCounts
        |> List.map (fun c -> if c > len / 2 then 1 else 0)

    let gamma = bitsToInt gammaBits

    let epsilon =
        gammaBits
        |> List.map (fun c -> 1 - c)
        |> bitsToInt

    gamma, epsilon

let run () : unit =
    let gamma, epsilon = gammaAndEpsilon data
    printfn $"gamma = {gamma}; epsilon = {epsilon}"
    printfn $"result = {gamma * epsilon}"
