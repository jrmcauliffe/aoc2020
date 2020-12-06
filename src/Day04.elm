module Day04 exposing (..)

import Dict exposing (..)



-- https://adventofcode.com/2020/day/4
-- Passport verification
--type alias Passport =
--    { byr : Maybe String
--    , iyr : Maybe String
--    , eyr : Maybe String
--    , hgt : Maybe String
--    , hcl : Maybe String
--    , ecl : Maybe String
--    , pid : Maybe String
--    , cid : Maybe String
--    }


type alias Passport =
    Dict String String


listToTuple2 : List a -> Maybe ( a, a )
listToTuple2 list =
    case list of
        [ a, b ] ->
            Just ( a, b )

        _ ->
            Nothing


parseRecord : String -> Passport
parseRecord s =
    --let
    --    fields =
    s |> String.replace "\n" " " |> String.split " " |> List.filterMap (\x -> x |> String.split ":" |> listToTuple2) |> Dict.fromList



--in
--Passport (Dict.get "byr" fields) (Dict.get "iyr" fields) (Dict.get "eyr" fields)
--         (Dict.get "hgt" fields) (Dict.get "hcl" fields) (Dict.get "ecl" fields)
--         (Dict.get "pid" fields) (Dict.get "cid" fields)


parseRecords : String -> List Passport
parseRecords s =
    s |> String.split "\n\n" |> List.map parseRecord


isValid : Passport -> Bool
isValid p =
    (p |> Dict.toList |> List.filter (\x -> Tuple.first x /= "cid") |> List.length) == 7
