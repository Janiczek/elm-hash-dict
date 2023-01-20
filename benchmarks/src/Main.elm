module Main exposing (..)

import Benchmark exposing (Benchmark)
import Benchmark.Alternative as Benchmark
import Benchmark.Runner.Alternative as Runner exposing (Program)
import Hash
import HashDict08Eviction


main : Program
main =
    Runner.program suite


itemsOfLength : Int -> List ( String, Int )
itemsOfLength n =
    List.range 1 n
        |> List.map (\v -> ( String.fromInt v, v ))


minExp =
    0


maxExp =
    12


prepare fromListFn =
    List.range minExp maxExp
        |> List.map
            (\exp ->
                let
                    length =
                        round (2 ^ toFloat exp)
                in
                ( String.fromInt length
                , itemsOfLength length |> fromListFn
                )
            )


test targetFn label fromListFn =
    prepare fromListFn
        |> List.map (\( name, target ) -> ( name, \_ -> targetFn target ))
        |> Benchmark.scale label


size sFn =
    test sFn


insertNotPresent iFn =
    test (\t -> iFn "" 999 t)


insertPresent iFn =
    test (\t -> iFn "1" 999 t)


getNotPresent gFn =
    test (\t -> gFn "" t)


getPresent gFn =
    test (\t -> gFn "1" t)


suite : Benchmark
suite =
    Benchmark.describe "Dictionaries"
        [ insertNotPresent HashDict08Eviction.insert "insert-not-present" (HashDict08Eviction.fromList Hash.string)
        , insertPresent HashDict08Eviction.insert "insert-present" (HashDict08Eviction.fromList Hash.string)
        , getNotPresent HashDict08Eviction.get "get-not-present" (HashDict08Eviction.fromList Hash.string)
        , getPresent HashDict08Eviction.get "get-present" (HashDict08Eviction.fromList Hash.string)
        ]
