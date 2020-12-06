module Day03 exposing (..)

import Array exposing (..)



-- https://adventofcode.com/2020/day/3
-- Toboggan navigation


type MapPoint
    = Tree
    | Clear


type alias Map =
    Array (Array MapPoint)


type alias Slope =
    ( Int, Int )


type alias Route =
    List MapPoint


width : Map -> Int
width m =
    m |> Array.get 0 |> Maybe.map Array.length |> Maybe.withDefault 0


length : Map -> Int
length m =
    Array.length m


parseMap : String -> Map
parseMap s =
    s
        |> String.split "\n"
        |> Array.fromList
        |> Array.map
            (\row ->
                row
                    |> String.toList
                    |> Array.fromList
                    |> Array.filter (\i -> i /= ' ')
                    |> Array.map
                        (\loc ->
                            if loc == '#' then
                                Tree

                            else
                                Clear
                        )
            )


traverse : Slope -> Map -> Route
traverse s m =
    let
        w =
            width m
    in
    m
        |> Array.toList
        |> List.indexedMap (\i -> \row -> ( i, Array.get (modBy w ((i // Tuple.first s) * Tuple.second s)) row |> Maybe.withDefault Clear ))
        -- select correct col and add row index
        |> List.filter (\tup -> modBy (Tuple.first s) (Tuple.first tup) == 0)
        |> List.map Tuple.second



-- Drop unused rows and remove index variable


traverseMulti : List Slope -> Map -> List Route
traverseMulti ls m =
    ls |> List.map (\s -> traverse s m)
