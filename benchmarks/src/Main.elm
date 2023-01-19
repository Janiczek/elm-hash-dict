module Main exposing (..)

import Benchmark exposing (Benchmark)
import Benchmark.Alternative as Benchmark
import Benchmark.Runner.Alternative as Runner exposing (Program)
import Dict
import Hash
import HashDict04WithoutClamp
import HashDict07RobinHood


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


d07 =
    HashDict07RobinHood.fromList Hash.string items


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


insertPresent steps label fromListFn insertFn =
    let
        ( key, value ) =
            ( String.fromInt (steps * 10)
            , 999
            )
    in
    Benchmark.scale label
        (prepare steps fromListFn
            |> List.map (\( name, target ) -> ( name, \_ -> insertFn key value target ))
        )


getNotPresent steps label fromListFn getFn =
    let
        key =
            String.fromInt (steps * 10 + 1)
    in
    Benchmark.scale label
        (prepare steps fromListFn
            |> List.map (\( name, target ) -> ( name, \_ -> getFn key target ))
        )


getPresent steps label fromListFn getFn =
    let
        key =
            String.fromInt (steps * 10)
    in
    Benchmark.scale label
        (prepare steps fromListFn
            |> List.map (\( name, target ) -> ( name, \_ -> getFn key target ))
        )


suite : Benchmark
suite =
    Benchmark.describe "Dictionaries"
        [ {-
             Benchmark.describe "size"
               [ size 15 "Dict" Dict.fromList Dict.size
               , size 15 "HashDict04WithoutClamp" (HashDict04WithoutClamp.fromList Hash.string) HashDict04WithoutClamp.size
               , size 15 "HashDict06LinearProbing" (HashDict06LinearProbing.fromList Hash.string) HashDict06LinearProbing.size
               , size 15 "HashDict07RobinHood" (HashDict07RobinHood.fromList Hash.string) HashDict07RobinHood.size
               ]
          -}
          Benchmark.describe "insert (not present)"
            [ insertNotPresent 15 "Dict" Dict.fromList Dict.insert
            , insertNotPresent 15 "HashDict07RobinHood" (HashDict07RobinHood.fromList Hash.string) HashDict07RobinHood.insert

            --[ insertNotPresent 15 "HashDict04WithoutClamp" (HashDict04WithoutClamp.fromList Hash.string) HashDict04WithoutClamp.insert
            ]
        , Benchmark.describe "insert (present)"
            [ insertPresent 15 "Dict" Dict.fromList Dict.insert
            , insertPresent 15 "HashDict07RobinHood" (HashDict07RobinHood.fromList Hash.string) HashDict07RobinHood.insert

            --[ insertPresent 15 "HashDict04WithoutClamp" (HashDict04WithoutClamp.fromList Hash.string) HashDict04WithoutClamp.insert
            ]
        , Benchmark.describe "get (not present)"
            [ getNotPresent 15 "Dict" Dict.fromList Dict.get
            , getNotPresent 15 "HashDict07RobinHood" (HashDict07RobinHood.fromList Hash.string) HashDict07RobinHood.get

            --[ getNotPresent 15 "HashDict04WithoutClamp" (HashDict04WithoutClamp.fromList Hash.string) HashDict04WithoutClamp.get
            ]
        , Benchmark.describe "get (present)"
            [ getPresent 15 "Dict" Dict.fromList Dict.get
            , getPresent 15 "HashDict07RobinHood" (HashDict07RobinHood.fromList Hash.string) HashDict07RobinHood.get

            --[ getPresent 15 "HashDict04WithoutClamp" (HashDict04WithoutClamp.fromList Hash.string) HashDict04WithoutClamp.get
            ]
        ]
