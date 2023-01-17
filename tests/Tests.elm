module Tests exposing (..)

import CommonTests.Dict
import Hash
import HashDict
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Janiczek/elm-hash-dict"
        [ CommonTests.Dict.isUnorderedDict
            { dictToString = Debug.toString
            , empty = Just <| HashDict.empty Hash.string
            , singleton = Just <| HashDict.singleton Hash.string
            , fromList = Just <| HashDict.fromList Hash.string
            , insert = Just HashDict.insert
            , update = Just HashDict.update
            , remove = Just HashDict.remove
            , map = Just HashDict.map
            , filter = Nothing
            , union = Nothing
            , intersect = Nothing
            , diff = Nothing
            , size = Just HashDict.size
            , isEmpty = Just HashDict.isEmpty
            , member = Just HashDict.member
            , get = Just HashDict.get
            , toList = Just HashDict.toList
            , foldl = Just HashDict.fold
            , foldr = Nothing
            , partition = Nothing
            , keys = Just HashDict.keys
            , values = Just HashDict.values
            , merge = Nothing
            }
        ]
