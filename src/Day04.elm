module Day04 exposing (..)

import Dict exposing (..)



-- https://adventofcode.com/2020/day/4
-- Passport verification


type HgtUnit
    = Cm
    | Inch


type alias Height =
    ( Int, HgtUnit )


type alias Passport =
    { byr : Maybe Int
    , iyr : Maybe Int
    , eyr : Maybe Int
    , hgt : Maybe Height
    , hcl : Maybe String
    , ecl : Maybe String
    , pid : Maybe String
    , cid : Maybe String
    }


listToTuple2 : List a -> Maybe ( a, a )
listToTuple2 list =
    case list of
        [ a, b ] ->
            Just ( a, b )

        _ ->
            Nothing



-- Part 1 treats the record as a simple Dictionary of Strings


parseRecordSimple : String -> Dict String String
parseRecordSimple s =
    s |> String.replace "\n" " " |> String.split " " |> List.filterMap (\x -> x |> String.split ":" |> listToTuple2) |> Dict.fromList


isValidSimple : Dict String String -> Bool
isValidSimple p =
    (p |> Dict.toList |> List.filter (\x -> Tuple.first x /= "cid") |> List.length) == 7



-- Part 2 has more involved validity checking during record construction


checkRange : Int -> Int -> Maybe Int -> Maybe Int
checkRange min max v =
    case v of
        Just y ->
            if (y > max) || (y < min) then
                Nothing

            else
                Just y

        Nothing ->
            Nothing


parseByr : String -> Maybe Int
parseByr s =
    s |> String.toInt |> checkRange 1920 2002


parseIyr : String -> Maybe Int
parseIyr s =
    s |> String.toInt |> checkRange 2010 2020


parseEyr : String -> Maybe Int
parseEyr s =
    s |> String.toInt |> checkRange 2020 2030


parseHgt : String -> Maybe Height
parseHgt s =
    case s |> String.toList |> List.partition Char.isDigit of
        ( xs, [ 'c', 'm' ] ) ->
            String.fromList xs |> String.toInt |> checkRange 150 193 |> Maybe.map (\h -> ( h, Cm ))

        ( xs, [ 'i', 'n' ] ) ->
            String.fromList xs |> String.toInt |> checkRange 59 76 |> Maybe.map (\h -> ( h, Inch ))

        _ ->
            Nothing


parseHcl : String -> Maybe String
parseHcl s =
    let
        xs =
            String.toList s
    in
    if
        List.head xs
            == Just '#'
            -- Begins with #
            && (List.tail xs |> Maybe.map (\l -> l |> List.filter (\c -> List.member c [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' ]) |> List.length))
            == Just 6
    then
        Just s

    else
        Nothing


parseEcl : String -> Maybe String
parseEcl s =
    if List.member s [ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" ] then
        Just s

    else
        Nothing


parsePid : String -> Maybe String
parsePid s =
    if (s |> String.toList |> List.filter Char.isDigit |> List.length) == 9 then
        Just s

    else
        Nothing


parseRecord : String -> Passport
parseRecord s =
    let
        fields =
            s |> String.replace "\n" " " |> String.split " " |> List.filterMap (\x -> x |> String.split ":" |> listToTuple2) |> Dict.fromList
    in
    Passport
        (Dict.get "byr" fields |> Maybe.andThen parseByr)
        (Dict.get "iyr" fields |> Maybe.andThen parseIyr)
        (Dict.get "eyr" fields |> Maybe.andThen parseEyr)
        (Dict.get "hgt" fields |> Maybe.andThen parseHgt)
        (Dict.get "hcl" fields |> Maybe.andThen parseHcl)
        (Dict.get "ecl" fields |> Maybe.andThen parseEcl)
        (Dict.get "pid" fields |> Maybe.andThen parsePid)
        (Dict.get "cid" fields)


parseRecords : String -> List Passport
parseRecords s =
    s |> String.split "\n\n" |> List.map parseRecord


isValid : Passport -> Bool
isValid p =
    p.byr
        /= Nothing
        && p.iyr
        /= Nothing
        && p.eyr
        /= Nothing
        && p.hgt
        /= Nothing
        && p.hcl
        /= Nothing
        && p.ecl
        /= Nothing
        && p.pid
        /= Nothing
