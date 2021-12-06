module AdventOfCode2021.Day2

open System
open Utilities

type Move =
    | Forward
    | Down
    | Up

type Instruction = { Move: Move; Distance: int }
type Position = { Horizontal: int; Depth: int }

let parseMove move =
    match move with
    | "forward" -> Forward
    | "up" -> Up
    | "down" -> Down
    | _ -> raise (Exception($"Unknown move: {move}"))

let parseLine (line: string) =
    let fields = line.Split(" ")

    { Move = parseMove fields.[0]
      Distance = fields.[1] |> int }

let instructions =
    readLines "Day2.txt" |> List.map parseLine

let move startPosition instruction =
    match instruction.Move with
    | Forward ->
        { startPosition with
              Horizontal = startPosition.Horizontal + instruction.Distance }
    | Up ->
        { startPosition with
              Depth = startPosition.Depth - instruction.Distance }
    | Down ->
        { startPosition with
              Depth = startPosition.Depth + instruction.Distance }

let rec processInstructions startPosition instructions =
    match instructions with
    | [] -> startPosition
    | head :: tail -> processInstructions (move startPosition head) tail

let run () : unit =
    let finalPosition =
        processInstructions { Horizontal = 0; Depth = 0 } instructions

    printfn $"End position: {finalPosition}"
    printfn $"Result: {finalPosition.Horizontal * finalPosition.Depth}"
