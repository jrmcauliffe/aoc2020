module Day09 exposing (..)

-- https://adventofcode.com/2020/day/8
-- Encoding Error

import Maybe.Extra exposing (..)


sumPairs : List Int -> List Int
sumPairs l =
    case l of
        [] ->
            []

        x :: [] ->
            []

        x :: xs ->
            sumPairs xs |> List.append (xs |> List.map (\i -> i + x))


attack : Int -> List Int -> Maybe Int
attack len xs =
    validate (xs |> List.take len) (xs |> List.drop len)


validate : List Int -> List Int -> Maybe Int
validate p l =
    let
        sum =
            sumPairs p

        n =
            l |> List.head

        t =
            l |> List.tail

        nextp =
            Maybe.map2 List.append (List.tail p) (n |> Maybe.map List.singleton)
    in
    case n |> Maybe.map (\x -> List.member x sum) of
        Just True ->
            Maybe.map2 validate nextp t |> Maybe.Extra.join

        -- Keep looking
        Just False ->
            n

        -- Found it
        Nothing ->
            Nothing


contigFind : Int -> List Int -> Maybe (List Int)
contigFind total l =
    case l of
        [] ->
            Nothing

        x :: [] ->
            if x == total then
                Just (List.singleton x)

            else
                Nothing

        x :: xs ->
            if x == total then
                Just (List.singleton x)

            else if x > total then
                Nothing

            else
                contigFind (total - x) xs |> Maybe.map (\next -> x :: next)


findMaxTwoSum : List Int -> Maybe Int
findMaxTwoSum l =
    let
        sorted =
            l |> List.sort

        min =
            sorted |> List.head

        max =
            sorted |> List.reverse |> List.head
    in
    Maybe.map2 (+) min max


findWeakness : Int -> List Int -> Maybe Int
findWeakness p l =
    case attack p l of
        Just k ->
            l |> List.indexedMap (\i -> \v -> l |> List.drop i) |> List.filterMap (contigFind k) |> List.head |> Maybe.map findMaxTwoSum |> Maybe.Extra.join

        Nothing ->
            Nothing
