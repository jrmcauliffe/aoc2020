module Day05 exposing (..)

import Binary exposing (fromIntegers, toDecimal)



-- https://adventofcode.com/2020/day/5
-- Binary Boarding


type alias BoardingPass =
    { row : Int, column : Int, id : Int }


parseBinary : Char -> Char -> String -> Int
parseBinary zeroChar oneChar s =
    s
        |> String.toList
        |> List.filterMap
            (\char ->
                if char == zeroChar then
                    Just 0

                else if char == oneChar then
                    Just 1

                else
                    Nothing
            )
        |> Binary.fromIntegers
        |> Binary.toDecimal


parseBP : String -> BoardingPass
parseBP s =
    let
        row =
            s |> parseBinary 'F' 'B'

        column =
            s |> parseBinary 'L' 'R'
    in
    BoardingPass row column (row * 8 + column)


findSeat : List BoardingPass -> Maybe BoardingPass
findSeat boardingPasses =
    let
        ids =
            boardingPasses |> List.map .id

        lowestId =
            ids |> List.sort |> List.head

        highestId =
            ids |> List.sort |> List.reverse |> List.head

        validIdRange =
            Maybe.map2 List.range lowestId highestId |> Maybe.withDefault []
    in
    validIdRange |> List.filter (\id -> not (List.member id ids)) |> List.head |> Maybe.map (\id -> BoardingPass (id // 8) (modBy 8 id) id)
