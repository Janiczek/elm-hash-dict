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


itemsOfLength : Int -> List ( String, Int )
itemsOfLength n =
    List.range 1 n
        |> List.map (\v -> ( String.fromInt v, v ))


items : List ( String, Int )
items =
    itemsOfLength 100


d00 =
    Dict.fromList items


d04 =
    HashDict04WithoutClamp.fromList Hash.string items


d06 =
    HashDict06LinearProbing.fromList Hash.string items


prepare steps fromListFn =
    List.range 0 steps
        |> List.map ((*) 10)
        |> List.map
            (\length ->
                ( String.fromInt length
                , itemsOfLength length |> fromListFn
                )
            )


size steps label fromListFn sizeFn =
    Benchmark.scale label
        (prepare steps fromListFn
            |> List.map (\( name, target ) -> ( name, \_ -> sizeFn target ))
        )


insertNotPresent steps label fromListFn insertFn =
    let
        ( key, value ) =
            ( String.fromInt (steps * 10 + 1)
            , 999
            )
    in
    Benchmark.scale label
        (prepare steps fromListFn
            |> List.map (\( name, target ) -> ( name, \_ -> insertFn key value target ))
        )


getPresent steps label fromListFn getFn =
    let
        key =
            String.fromInt (steps * 10 - 1)
    in
    Benchmark.scale label
        (prepare steps fromListFn
            |> List.map (\( name, target ) -> ( name, \_ -> getFn key target ))
        )


suite : Benchmark
suite =
    Benchmark.describe "Dictionaries"
        [ {-
             Benchmark.rank "fromList >> toList"
               (\f -> f items)
               [ ( "Dict", Dict.fromList >> Dict.toList )
               , ( "HashDict04WithoutClamp", HashDict04WithoutClamp.fromList Hash.string >> HashDict04WithoutClamp.toList )
               , ( "HashDict06LinearProbing", HashDict06LinearProbing.fromList Hash.string >> HashDict06LinearProbing.toList )
               ]
          -}
          {-
             Benchmark.rank "get (successful)"
                 (\f -> f "50")
                 [ ( "Dict", \k -> Dict.get k d00 )
                 , ( "HashDict04WithoutClamp", \k -> HashDict04WithoutClamp.get k d04 )
                 , ( "HashDict06LinearProbing", \k -> HashDict06LinearProbing.get k d06 )
                 ]
          -}
          {-
             Benchmark.rank "get (unsuccessful)"
                 (\f -> f "150")
                 [ ( "Dict", \k -> Dict.get k d00 )
                 , ( "HashDict04WithoutClamp", \k -> HashDict04WithoutClamp.get k d04 )
                 , ( "HashDict06LinearProbing", \k -> HashDict06LinearProbing.get k d06 )
                 ]
          -}
          {-
             Benchmark.describe "size"
               [ size 15 "Dict" Dict.fromList Dict.size
               , size 15 "HashDict04WithoutClamp" (HashDict04WithoutClamp.fromList Hash.string) HashDict04WithoutClamp.string
               , size 15 "HashDict06LinearProbing" (HashDict06LinearProbing.fromList Hash.string) HashDict06LinearProbing.string
               ]
          -}
          {-
             Benchmark.describe "insert (not present)"
               [ insertNotPresent 15 "Dict" Dict.fromList Dict.insert
               , insertNotPresent 15 "HashDict04WithoutClamp" (HashDict04WithoutClamp.fromList Hash.string) HashDict04WithoutClamp.insert
               , insertNotPresent 15 "HashDict06LinearProbing" (HashDict06LinearProbing.fromList Hash.string) HashDict06LinearProbing.insert
               ]
          -}
          Benchmark.describe "get (present)"
            [ getPresent 15 "Dict" Dict.fromList Dict.get
            , getPresent 15 "HashDict04WithoutClamp" (HashDict04WithoutClamp.fromList Hash.string) HashDict04WithoutClamp.get
            , getPresent 15 "HashDict06LinearProbing" (HashDict06LinearProbing.fromList Hash.string) HashDict06LinearProbing.get
            ]
        ]
