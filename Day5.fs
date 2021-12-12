module AdventOfCode2021.Day5

open Utilities
open System
open Microsoft.FSharp.Collections

type Point = { X: int; Y: int }
type Line = { Point1: Point; Point2: Point }

let lineToPoints line =
    if line.Point1.X = line.Point2.X then
        let yMin, yMax =
            min line.Point1.Y line.Point2.Y, max line.Point1.Y line.Point2.Y

        [ yMin .. yMax ]
        |> List.map (fun y -> { X = line.Point1.X; Y = y })
    elif line.Point1.Y = line.Point2.Y then
        let xMin, xMax =
            min line.Point1.X line.Point2.X, max line.Point1.X line.Point2.X

        [ xMin .. xMax ]
        |> List.map (fun x -> { X = x; Y = line.Point1.Y })
    else
        failwith $"Not a straight line: {line}"

type Grid private (cells: int array, width: int) =
    let height = cells.Length / width

    do
        if width * height <> cells.Length then
            failwith $"Incorrect cells size: {cells.Length}"

    let pointToIndex point = point.Y * width + point.X

    let grow newWidth newHeight =
        let extraWidth = newWidth - width
        let extraHeight = newHeight - height

        if extraWidth < 0 then
            failwith $"Cannot shrink width: {newWidth} < {width}"

        if extraHeight < 0 then
            failwith $"Cannot shrink height: {newHeight} < {height}"

        let rows = cells |> Array.chunkBySize width

        let newRows =
            rows
            |> Array.map
                (fun r ->
                    Array.concat [ r
                                   Array.zeroCreate extraWidth ])

        let newColumns =
            [| 1 .. extraHeight |]
            |> Array.map (fun _ -> Array.zeroCreate newWidth)

        let newCells =
            Array.concat [ newRows; newColumns ]
            |> Array.collect id

        Grid(newCells, newWidth)

    new() = Grid(Array.zeroCreate 1, 1)

    member private this.Fitting(line: Line) =
        let xMax =
            max width ((max line.Point1.X line.Point2.X) + 1)

        let yMax =
            max height ((max line.Point1.Y line.Point2.Y) + 1)

        if xMax > width || yMax > height then
            grow xMax yMax
        else
            this

    member private this.Mark(point: Point) =
        let index = pointToIndex point
        cells.[index] <- cells.[index] + 1
        this

    member this.AddLine(line: Line) =
        let grid = this.Fitting(line)

        lineToPoints line
        |> List.fold (fun (g: Grid) -> g.Mark) grid

    member this.GetOverlappingValues() = cells |> List.ofArray

    override this.ToString() =
        let rowToString row =
            row
            |> Array.map string
            |> Array.map (fun s -> if s = "0" then "." else s)
            |> String.concat ""

        cells
        |> Array.chunkBySize width
        |> Array.map rowToString
        |> String.concat Environment.NewLine

let parsePoint (str: string) =
    match str.Split(",") with
    | [| x; y |] -> { X = int x; Y = int y }
    | _ -> failwith $"Invalid point: {str}"

let parseLine (str: string) =
    match str.Split(" -> ") with
    | [| p1; p2 |] ->
        { Point1 = parsePoint p1
          Point2 = parsePoint p2 }
    | _ -> failwith $"Incorrect input: {str}"

let testData =
    [ "0,9 -> 5,9"
      "8,0 -> 0,8"
      "9,4 -> 3,4"
      "2,2 -> 2,1"
      "7,0 -> 7,4"
      "6,4 -> 2,0"
      "0,9 -> 2,9"
      "3,4 -> 1,4"
      "0,0 -> 8,8"
      "5,5 -> 8,2" ]

let data = readLines "Day5.txt"

let run () : unit =
    let lines = data |> List.map parseLine

    let grid =
        lines
        |> List.filter (fun l -> l.Point1.X = l.Point2.X || l.Point1.Y = l.Point2.Y)
        |> List.fold (fun (g: Grid) -> g.AddLine) (Grid())

    printfn $"{grid}"

    let countMultipleOverlapping =
        grid.GetOverlappingValues()
        |> List.filter (fun c -> c >= 2)
        |> List.length

    printfn $"Number of multiple overlapping points: {countMultipleOverlapping}"
    ()
