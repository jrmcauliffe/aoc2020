module Day10 exposing (..)

-- https://adventofcode.com/2020/day/10
-- Adaptor Array

import Maybe.Extra exposing (..)


diffList : List Int -> List Int
diffList l =
    let
        sorted =
            l |> List.sort

        max =
            sorted |> List.reverse |> List.head

        listA =
            max |> Maybe.map (\m -> List.append sorted [ m + 3 ]) |> Maybe.withDefault []

        listB =
            0 :: sorted
    in
    List.map2 (\a -> \b -> a - b) listA listB


diffMult : List Int -> Int
diffMult l =
    let
        df =
            l |> diffList

        ones =
            df |> List.filter ((==) 1)

        threes =
            df |> List.filter ((==) 3)
    in
    (ones |> List.length) * (threes |> List.length)
