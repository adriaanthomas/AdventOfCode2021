module AdventOfCode2021.Day2

open Utilities

type Move =
    | Forward
    | Down
    | Up

type Instruction =
    { Move: Move
      Distance: int }
    override x.ToString() =
        $"{{ Move={x.Move}; Distance={x.Distance} }}"

type Position =
    { Horizontal: int
      Depth: int
      Aim: int }
    override x.ToString() =
        $"{{ Horizontal={x.Horizontal}; Depth={x.Depth}; Aim={x.Aim} }}"

let parseMove move =
    match move with
    | "forward" -> Forward
    | "up" -> Up
    | "down" -> Down
    | _ -> failwith $"Unknown move: {move}"

let parseLine (line: string) =
    let fields = line.Split(" ")

    { Move = parseMove fields.[0]
      Distance = fields.[1] |> int }

let instructions =
    readLines "Day2.txt" |> List.map parseLine

let testInstructions =
    [ "forward 5"
      "down 5"
      "forward 8"
      "up 3"
      "down 8"
      "forward 2" ]
    |> List.map parseLine

let logPosition position =
    printfn $"{position}"
    position

let move startPosition instruction =
    match instruction.Move with
    | Forward ->
        { startPosition with
              Horizontal = startPosition.Horizontal + instruction.Distance
              Depth =
                  startPosition.Depth
                  + startPosition.Aim * instruction.Distance }
    | Up ->
        { startPosition with
              Aim = startPosition.Aim - instruction.Distance }
    | Down ->
        { startPosition with
              Aim = startPosition.Aim + instruction.Distance }

let rec processInstructions startPosition instructions =
    match instructions with
    | [] -> startPosition
    | instruction :: tail ->
        let newPosition = move startPosition instruction
        printfn $"{startPosition} -> {instruction} -> {newPosition}"
        processInstructions newPosition tail

let run () : unit =
    let finalPosition =
        processInstructions { Horizontal = 0; Depth = 0; Aim = 0 } instructions

    printfn $"End position: {finalPosition}"
    printfn $"Result: {finalPosition.Horizontal * finalPosition.Depth}"
