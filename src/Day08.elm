module Day08 exposing (..)

-- https://adventofcode.com/2020/day/8
-- Game Machine

import Array exposing (Array)
import Maybe.Extra exposing (..)


type alias Address =
    Int


type Instuction
    = ACC Int
    | JMP Int
    | NOP Int


type alias Program =
    Array Instuction


type alias State =
    { memory : Program
    , pc : Address
    , acc : Int
    , altered : Bool
    , prev : List Int
    }


parseInstruction : String -> Maybe Instuction
parseInstruction s =
    case s |> String.replace "+" " " |> String.words of
        [ "acc", i ] ->
            String.toInt i |> Maybe.map (\j -> ACC j)

        [ "jmp", i ] ->
            String.toInt i |> Maybe.map (\j -> JMP j)

        [ "nop", i ] ->
            String.toInt i |> Maybe.map (\j -> NOP j)

        _ ->
            Nothing


-- Execute the instruction pointed to by the pc, update the pc and acc
-- if we ever run an already run instruction, halt
run : State -> State
run s =
    if s.prev |> List.member s.pc then
        s

    else
        case s.memory |> Array.get s.pc of
            Just (ACC x) ->
                { s | prev = s.pc :: s.prev, pc = s.pc + 1, acc = s.acc + x } |> run

            Just (JMP x) ->
                { s | prev = s.pc :: s.prev, pc = s.pc + x } |> run

            Just (NOP x) ->
                { s | prev = s.pc :: s.prev, pc = s.pc + 1 } |> run

            Nothing ->
                s

-- One JMP or NOP instruction is switched and there is only one solution that advances pc past the end of memory
-- At each JMP or NOP, try both instructions, tracking the change with .altered (only one instruction is corrupted)
-- Only one of the two should be not Nothing given the constraints, so combine with Maybe.Extra |> orElse
run2 : State -> Maybe State
run2 s =
    if s.prev |> List.member s.pc then
        Nothing

    else if s.pc == Array.length s.memory then
        Just s

    else
        case ( s.altered, s.memory |> Array.get s.pc ) of
            ( _, Just (ACC x) ) ->
                { s | prev = s.pc :: s.prev, pc = s.pc + 1, acc = s.acc + x } |> run2

            ( True, Just (JMP x) ) ->
                { s | prev = s.pc :: s.prev, pc = s.pc + x } |> run2

            ( False, Just (JMP x) ) ->
                { s | prev = s.pc :: s.prev, pc = s.pc + x }
                    |> run2
                    |> orElse ({ s | altered = True, prev = s.pc :: s.prev, pc = s.pc + 1 } |> run2)

            ( True, Just (NOP x) ) ->
                { s | prev = s.pc :: s.prev, pc = s.pc + 1 } |> run2

            ( False, Just (NOP x) ) ->
                { s | prev = s.pc :: s.prev, pc = s.pc + 1 }
                    |> run2
                    |> orElse ({ s | altered = True, prev = s.pc :: s.prev, pc = s.pc + x } |> run2)

            ( _, Nothing ) ->
                Nothing

