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
            , filter = Just HashDict.filter
            , union = Just HashDict.union
            , intersect = Just HashDict.intersect
            , diff = Just HashDict.diff
            , size = Just HashDict.size
            , isEmpty = Just HashDict.isEmpty
            , member = Just HashDict.member
            , get = Just HashDict.get
            , toList = Just HashDict.toList
            , foldl = Just HashDict.fold
            , foldr = Nothing
            , partition = Just HashDict.partition
            , keys = Just HashDict.keys
            , values = Just HashDict.values
            , merge = Nothing
            }
        ]
