module Day10Test exposing (..)

import Day10 exposing (..)
import Expect exposing (equal)
import List exposing (filterMap)
import Test exposing (..)

example1 =  [16, 10, 15, 5, 1, 11, 7, 19, 6, 12,4]
example2 = [28,33,18,42,31,14,46,20,48,47,24,23,49,45,19,38,39,11,1,32,25,35,8,17,7,9,4,2,34,10,3]
problem = [54,91,137,156,31,70,143,51,50,18,1,149,129,151,95,148,41,144,7,125,155,14,114,108,57,118,147,24,25,73,26,8,115,44,12,47,106,120,132,121,35,105,60,9,6,65,111,133,38,138,101,126,39,78,92,53,119,136,154,140,52,15,90,30,40,64,67,139,76,32,98,113,80,13,104,86,27,61,157,79,122,59,150,89,158,107,77,112,5,83,58,21,2,66]
suite : Test
suite =
    describe "Day 10 Tests"
        [ describe "Part 1"
            [ test "Example 1" <|
                \_ -> example1 |> diffMult |> equal 35
            , test "Example 2" <|
                \_ -> example2 |> diffMult |> equal 220
            , test "Problem Part 1" <|
                \_ -> problem |> diffMult |> equal 2046
            ]
        ]
