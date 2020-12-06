module Day01 exposing (..)

-- https://adventofcode.com/2020/day/1
-- Find two entries that sum to 2020


sumPair2020 : List Int -> Maybe (List Int)
sumPair2020 expenses =
    expenses
        |> List.concatMap (\e1 -> expenses |> List.map (\e2 -> [ e1, e2 ]))
        |> List.filter (\a -> List.foldr (+) 0 a == 2020)
        |> List.head



-- Find three entries that sum to 2020


sumTriple2020 : List Int -> Maybe (List Int)
sumTriple2020 expenses =
    expenses
        |> List.concatMap (\e1 -> expenses |> List.concatMap (\e2 -> expenses |> List.map (\e3 -> [ e1, e2, e3 ])))
        |> List.filter (\a -> List.foldr (+) 0 a == 2020)
        |> List.head
