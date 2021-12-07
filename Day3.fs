module AdventOfCode2021.Day3

open Utilities

type Bit = bool

type Bits = Bit array

let parseBits (line: string) : Bits =
    let charToBit ch =
        match ch with
        | '1' -> true
        | '0' -> false
        | _ -> failwith $"Not a bit character: {ch}"

    line.ToCharArray() |> Array.map charToBit

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
    |> List.map parseBits

let data =
    readLines "Day3.txt" |> List.map parseBits

let countBit bit = if bit then 0, 1 else 1, 0

let mostCommonBit (zeros, ones) = ones >= zeros
let leastCommonBit (zeros, ones) = ones < zeros

let addCounts (c10, c11) (c20, c21) = c10 + c20, c11 + c21

let mostCommonBits (values: Bits list) : Bits =
    let countBits bits = bits |> Array.map countBit

    let addBitCounts counts1 counts2 =
        Array.zip counts1 counts2
        |> Array.map (fun (c1, c2) -> addCounts c1 c2)

    values
    |> List.map countBits
    |> List.reduce addBitCounts
    |> Array.map mostCommonBit

let bitsAt (index: int) (values: Bits list) : Bit list = values |> List.map (fun b -> b.[index])

let commonBitAt (bitSelector: int * int -> Bit) (index: int) (values: Bits list) : Bit =
    values
    |> bitsAt index
    |> List.map countBit
    |> List.reduce addCounts
    |> bitSelector

let bitsToInt (bits: Bits) : int =
    let bitToInt index bit =
        if bit then
            1 <<< bits.Length - 1 - index
        else
            0

    bits |> Array.mapi bitToInt |> Array.sum

let inverseBits (bits: Bits) : Bits = bits |> Array.map not

let gammaAndEpsilon (data: Bits list) =
    let mostCommon = mostCommonBits data
    let gamma = mostCommon |> bitsToInt
    let epsilon = mostCommon |> inverseBits |> bitsToInt

    gamma, epsilon

let oxygenGeneratorAndCO2ScrubberRatings (data: Bits list) =
    //    let bitToString bit = if bit then "1" else "0"
//
//    let bitsToString bits =
//        bits
//        |> Array.map bitToString
//        |> Array.fold (fun a b -> a + b) ""
//
//    let bitsListToString values =
//        values
//        |> List.map bitsToString
//        |> String.concat ", "

    let rec filterCommonAt (bitSelector: int * int -> Bit) (index: int) (values: Bits list) =
        //        printfn $"filterCommonAt {denominator} {index} {bitsListToString values}"

        match values with
        | [] -> failwith "Not enough data"
        | [ x ] -> x
        | _ ->
            let common = values |> commonBitAt bitSelector index

            let remaining =
                values
                |> List.filter (fun v -> v.[index] = common)

            filterCommonAt bitSelector (index + 1) remaining

    let filterMostCommon = filterCommonAt mostCommonBit 0
    let filterLeastCommon = filterCommonAt leastCommonBit 0
    let oxygenGenerator = data |> filterMostCommon |> bitsToInt
    let co2ScrubberRatings = data |> filterLeastCommon |> bitsToInt

    oxygenGenerator, co2ScrubberRatings

let run () : unit =
    let values = testData
    let gamma, epsilon = gammaAndEpsilon values
    printfn $"gamma = {gamma}; epsilon = {epsilon}"
    printfn $"power consumption = {gamma * epsilon}"
    printfn ""

    let ogr, csr =
        oxygenGeneratorAndCO2ScrubberRatings values

    printfn $"oxygen generator rating = {ogr}; CO2 scrubber rating = {csr}"
    printfn $"life support rating = {ogr * csr}"
