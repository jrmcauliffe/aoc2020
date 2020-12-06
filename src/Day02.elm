module Day02 exposing (..)

import Array exposing (..)



-- https://adventofcode.com/2020/day/2
-- Find Valid passwords from rules


type alias LetterRange =
    { min : Int, max : Int }


type alias PasswordRule =
    { letter : Char, range : LetterRange }


type alias Password =
    String


parseLetterRange : String -> Maybe LetterRange
parseLetterRange s =
    case s |> String.split "-" |> List.map String.toInt of
        [ Just mn, Just mx ] ->
            Just { min = mn, max = mx }

        _ ->
            Nothing


parse : String -> Maybe ( PasswordRule, Password )
parse s =
    case String.split " " s of
        [ cnt, l, pw ] ->
            Maybe.map2 (\r -> \c -> ( { letter = c, range = r }, pw )) (parseLetterRange cnt) (l |> String.toList |> List.head)

        _ ->
            Nothing



-- Is the number of the given char within the given range?


isValid : ( PasswordRule, Password ) -> Bool
isValid ( rule, pass ) =
    let
        letterCount =
            pass |> String.toList |> List.filter (\c -> c == rule.letter) |> List.length
    in
    letterCount <= rule.range.max && letterCount >= rule.range.min



-- Does one and only one of the two positions contain the character?


isValidNew : ( PasswordRule, Password ) -> Bool
isValidNew ( rule, pass ) =
    let
        letters =
            pass |> String.toList |> Array.fromList
    in
    Maybe.map2 (\a -> \b -> xor (a == rule.letter) (b == rule.letter)) (Array.get (rule.range.min - 1) letters) (Array.get (rule.range.max - 1) letters)
        |> Maybe.withDefault False
