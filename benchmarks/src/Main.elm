module Main exposing (..)

import Benchmark exposing (Benchmark)
import Benchmark.Alternative as Benchmark
import Benchmark.Runner.Alternative as Runner exposing (Program)
import Dict
import Hash
import HashDict04WithoutClamp
import HashDict06LinearProbing


main : Program
main =
    Runner.program suite


items : List ( String, Int )
items =
    List.range 1 100
        |> List.map (\v -> ( String.fromInt v, v ))


d00 =
    Dict.fromList items


d04 =
    HashDict04WithoutClamp.fromList Hash.string items


d06 =
    HashDict06LinearProbing.fromList Hash.string items


suite : Benchmark
suite =
    Benchmark.describe "Dictionaries"
        [ Benchmark.rank "fromList"
            (\f -> f items)
            [ ( "Dict", Dict.fromList >> Dict.toList )
            , ( "HashDict04WithoutClamp", HashDict04WithoutClamp.fromList Hash.string >> HashDict04WithoutClamp.toList )
            , ( "HashDict06LinearProbing", HashDict06LinearProbing.fromList Hash.string >> HashDict06LinearProbing.toList )
            ]
        , Benchmark.rank "get"
            (\f -> f "150")
            [ ( "Dict", \k -> Dict.get k d00 )
            , ( "HashDict04WithoutClamp", \k -> HashDict04WithoutClamp.get k d04 )
            , ( "HashDict06LinearProbing", \k -> HashDict06LinearProbing.get k d06 )
            ]
        ]
