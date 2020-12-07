module Day07 exposing (..)

-- https://adventofcode.com/2020/day/7
-- Bag nonsense


type alias BagName =
    String


type alias BagContents =
    List ( Int, BagName )


type alias BagRule =
    ( BagName, BagContents )


parseSubRule : String -> Maybe ( Int, BagName )
parseSubRule s =
    let
        tokens =
            s |> String.words

        quan =
            tokens |> List.head |> Maybe.andThen String.toInt

        name =
            tokens |> List.tail |> Maybe.map (\l -> l |> List.take (List.length tokens - 2) |> List.intersperse " " |> String.concat)
    in
    Maybe.map2 (\q -> \n -> ( q, n )) quan name


parseBagRule : String -> Maybe BagRule
parseBagRule s =
    let
        bagName =
            s |> String.split "contain" |> List.head |> Maybe.map (String.dropRight 6)

        bagContents =
            s |> String.split "contain" |> List.tail |> Maybe.withDefault [] |> List.map (String.split ",") |> List.concat |> List.filterMap parseSubRule
    in
    bagName |> Maybe.map (\name -> ( name, bagContents ))


getContents : BagName -> List BagRule -> BagContents
getContents b brl =
    case brl |> List.filter (\x -> Tuple.first x == b) of
        ( _, x ) :: [] ->
            x

        _ ->
            []


allInsideFlat : BagName -> List BagRule -> BagContents
allInsideFlat b br =
    let
        contents =
            getContents b br
    in
    contents |> List.map (\y -> [ y ] ++ allInsideFlat (Tuple.second y) br) |> List.foldr (++) []


countInside : BagName -> List BagRule -> Int
countInside b br =
    let
        contents =
            getContents b br

        contentsCount =
            contents |> List.map (\bc -> Tuple.first bc * countInside (Tuple.second bc) br) |> List.sum
    in
    contentsCount + 1


containsCount : BagName -> List BagRule -> Int
containsCount b bl =
    bl |> List.map (\x -> allInsideFlat (Tuple.first x) bl) |> List.filter (\l -> l |> List.map Tuple.second |> List.member b) |> List.length


insideCount : BagName -> List BagRule -> Int
insideCount b bl =
    (bl |> countInside b) - 1
