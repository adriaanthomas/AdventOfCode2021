module AdventOfCode2021.Day4

open System
open Utilities

type Position = { Column: int; Row: int }
type BoardCell = { Value: int; Set: bool }

type BingoCheckOutcome =
    | Bingo
    | NotYet

type Board private (cells: BoardCell array, columns: int) =
    let rows = cells.Length / columns

    do
        if rows * columns <> cells.Length then
            failwith $"Incorrect cells size: {cells.Length}"

    let indexToPosition index =
        { Column = index / columns
          Row = index % columns }

    let positionToIndex =
        cells
        |> Array.mapi (fun i c -> indexToPosition i, i)
        |> Map.ofArray

    let valueToIndex =
        cells
        |> Array.mapi (fun i c -> c.Value, i)
        |> Map.ofArray

    do
        if valueToIndex.Count <> cells.Length then
            failwith $"Not all values are unique: %A{cells |> Array.map (fun c -> c.Value)}"

    let row index =
        [ 0 .. columns - 1 ]
        |> List.map (fun c -> positionToIndex.[{ Column = c; Row = index }])
        |> List.map (fun i -> cells.[i])

    let column index =
        [ 0 .. rows - 1 ]
        |> List.map (fun r -> positionToIndex.[{ Column = index; Row = r }])
        |> List.map (fun i -> cells.[i])

    let bingoOnRow position =
        row position.Row |> List.forall (fun c -> c.Set)

    let bingoOnColumn position =
        column position.Column
        |> List.forall (fun c -> c.Set)

    let hasBingo position =
        if bingoOnRow position || bingoOnColumn position then
            Bingo
        else
            NotYet

    new(lines: string list) =
        let values =
            lines
            |> Array.ofList
            |> Array.map
                (fun l ->
                    l.Split(
                        " ",
                        StringSplitOptions.TrimEntries
                        ||| StringSplitOptions.RemoveEmptyEntries
                    ))
            |> Array.map (Array.map int)

        let columns = values.[0].Length

        let cells =
            values
            |> Array.collect id
            |> Array.map (fun v -> { Value = v; Set = false })

        Board(cells, columns)

    member private this.Set(index: int) =
        cells.[index] <- { cells.[index] with Set = true }

    member this.SetAt(position: Position) =
        let index = positionToIndex.[position]
        this.Set(index)
        hasBingo position

    member this.SetValue(value: int) =
        match Map.tryFind value valueToIndex with
        | Some index ->
            this.Set(index)
            hasBingo (indexToPosition index)
        | None -> NotYet

    member this.GetUnsetValues() =
        cells
        |> Array.filter (fun c -> not c.Set)
        |> Array.map (fun c -> c.Value)

    override this.ToString() =
        cells
        |> Array.map (fun c -> c.Value)
        |> Array.chunkBySize columns
        |> Array.map
            (fun r ->
                r
                |> Array.map (fun v -> $"%2i{v}")
                |> String.concat " ")
        |> String.concat Environment.NewLine

let lines = readLines "Day4.txt"

let testLines =
    [ "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
      ""
      "22 13 17 11  0"
      " 8  2 23  4 24"
      "21  9 14 16  7"
      " 6 10  3 18  5"
      " 1 12 20 15 19"
      ""
      " 3 15  0  2 22"
      " 9 18 13 17  5"
      "19  8  7 25 23"
      "20 11 10 24  4"
      "14 21 16 12  6"
      ""
      "14 21 17 24  4"
      "10 16 15  9 19"
      "18  8 23 26 20"
      "22 11 13  6  5"
      " 2  0 12  3  7" ]

let parseLines (lines: string list) =
    let valueLine, boardLines =
        match lines with
        | head :: tail -> head, tail
        | _ -> failwith "Not enough input lines"

    let values =
        valueLine.Split(",")
        |> Array.map int
        |> List.ofArray

    let mkBoards (boards, currentLines) line =
        match line with
        | "" when List.isEmpty currentLines -> boards, currentLines
        | "" -> Board(List.rev currentLines) :: boards, List.empty
        | _ -> boards, line :: currentLines

    let mkFinalBoard acc =
        let boards, _ = mkBoards acc ""
        List.rev boards

    let boards =
        boardLines
        |> List.fold mkBoards (List.empty, List.empty)
        |> mkFinalBoard

    values, boards

let printBoards boards =
    boards
    |> List.iter (fun b -> printfn $"{b}{Environment.NewLine}")

let rec findWinningBoardsAndValue (boards: Board list) values =
    match values with
    | [] -> None
    | value :: remainingValues ->
        let bingoBoards =
            boards
            |> List.filter (fun b -> b.SetValue(value) = Bingo)

        match bingoBoards with
        | [ x ] -> Some([ x ], value)
        | x :: xs -> Some(x :: xs, value)
        | [] -> findWinningBoardsAndValue boards remainingValues

let run () : unit =
    let values, boards = parseLines lines
    printBoards boards

    match findWinningBoardsAndValue boards values with
    | None -> failwith "Not enough values for any board to win"
    | Some ([ board ], value) -> printfn $"Winning: {(board.GetUnsetValues() |> Array.sum) * value}"
    | _ -> failwith "More than one board won, not supported"
