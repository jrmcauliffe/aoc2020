module Day06 exposing (..)

import Set exposing (..)



-- https://adventofcode.com/2020/day/6
-- Customs Declaration
--Anyone has answered yes, combine and form a set


parseGroup : String -> Set Char
parseGroup s =
    s |> String.toList |> List.filter (\c -> c /= '\n') |> Set.fromList



--Everyone has answered yes, find intersection of each person in group


parseGroupNew : String -> Set Char
parseGroupNew s =
    let
        answers =
            s |> String.split "\n" |> List.map (\line -> line |> String.toList |> Set.fromList)
    in
    case answers of
        x :: [] ->
            x

        x :: xs ->
            List.foldr Set.intersect x xs

        _ ->
            Set.empty
