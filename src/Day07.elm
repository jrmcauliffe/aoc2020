module Day07 exposing (..)

-- https://adventofcode.com/2020/day/7
-- Bag nonsense

type alias Bag = String
type alias BagContents = List (Int, Bag)
type alias BagRule = (Bag, BagContents )

parseSubRule : String -> Maybe (Int, Bag)
parseSubRule s = let
                     tokens = s |> String.words
                     quan =  tokens |> List.head |> Maybe.andThen String.toInt
                     name = tokens |> List.tail |> Maybe.map (\l -> l |> List.take ((List.length tokens) - 2 ) |> List.intersperse " " |> String.concat)
                 in
                 Maybe.map2 (\q -> \n -> (q, n)) quan name

parseBagRule : String -> Maybe BagRule
parseBagRule s =
    let
        bagName = s |> String.split "contain" |> List.head |> Maybe.map (String.dropRight 6)
        bagContents = s |> String.split "contain" |> List.tail |> Maybe.withDefault [] |> List.map (String.split ",") |> List.concat |> List.filterMap parseSubRule
    in
   bagName |> Maybe.map (\name -> (name, bagContents))


allInside :  Bag -> List BagRule -> BagContents
allInside b br =
    case br |> List.filter (\x -> Tuple.first x == b) of
    (_, x) :: [] -> x |> List.map (\y -> [y] ++ (allInside (Tuple.second y) br) ) |> List.foldr (++) []
    _ -> []

countInside :  Bag -> List BagRule  -> Int
countInside b br =
    case br |> List.filter (\x -> Tuple.first x == b) of
    (_, x) :: [] -> x |> List.map (\y -> Tuple.first y + (countInside (Tuple.second y) br) ) |> List.sum
    _ -> 0


containsCount : Bag -> List BagRule -> Int
containsCount b bl = bl |> List.map (\x -> allInside (Tuple.first x) bl) |> List.filter (\l -> l |> List.map Tuple.second |> List.member b) |> List.length

insideCount : Bag -> List BagRule -> Int
insideCount b bl = bl |> countInside b

example = """light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."""

