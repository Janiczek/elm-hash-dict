module HashDict exposing
    ( Dict
    , empty
    , fromList
    , get
    , insert
    )

import Array exposing (Array)
import Bitwise
import ListExtra as List


type Dict k v
    = Dict (Inner k v)


type alias Inner k v =
    { hash : k -> Int
    , buckets : Array (List ( k, v ))
    , size : Int -- actual keys stored
    , usedBuckets : Int
    , bucketsCapacity : Int -- derivable from bucketsShift, but let's cache it. Used in every resize check (after insert)
    , bucketsShift : Int
    }


initBucketsShift : Int
initBucketsShift =
    -- TODO PERF: measure the best value?
    -- --> shift by this amount == modulo initBucketsCount
    -- example: >>> 30 == % 4 if the hash is 32bit
    30


initBucketsCount : Int
initBucketsCount =
    -- if shift == 30, count will be 4
    2 ^ (32 - initBucketsShift)


{-| fill threshold is a percentage expressed in k \* (count >>> d)
for example, 0.75 = 75% = \* 3 / 4... we leave 3 as is, and express 4 as >>> 2.
thus 0.75 === (3,2)
-}
fillThresholdMul : Int
fillThresholdMul =
    3


fillThresholdShift : Int
fillThresholdShift =
    2


empty : (k -> Int) -> Dict k v
empty hash =
    Dict (emptyInner hash)


emptyInner : (k -> Int) -> Inner k v
emptyInner hash =
    { hash = \k -> Bitwise.and 0xFFFFFFFF (hash k) -- 32 bit
    , buckets = Array.repeat initBucketsCount []
    , size = 0
    , usedBuckets = 0
    , bucketsCapacity = initBucketsCount
    , bucketsShift = initBucketsShift
    }


bucketIndex : Int -> Int -> Int
bucketIndex hash shift =
    Bitwise.shiftRightZfBy shift hash


addEntry : ( k, v ) -> Inner k v -> Inner k v
addEntry ( key, value ) dict =
    -- without resizing
    let
        insertLoop : List ( k, v ) -> List ( k, v ) -> ( List ( k, v ), Int )
        insertLoop acc remaining =
            case remaining of
                [] ->
                    ( ( key, value ) :: acc
                    , dict.size + 1
                    )

                (( xk, _ ) as x) :: xs ->
                    if xk == key then
                        ( (( xk, value ) :: acc) ++ xs
                        , dict.size
                        )

                    else
                        insertLoop (x :: acc) xs

        index =
            bucketIndex (dict.hash key) dict.bucketsShift
    in
    case Array.get index dict.buckets of
        Nothing ->
            Debug.todo "can't happen if bucketsCount and bucketIndex are implemented correctly"

        Just collisions ->
            let
                ( newBucket, newSize ) =
                    insertLoop [] collisions

                newUsedBuckets =
                    if List.isEmpty collisions then
                        dict.usedBuckets + 1

                    else
                        dict.usedBuckets
            in
            { dict
                | buckets = Array.set index newBucket dict.buckets
                , usedBuckets = newUsedBuckets
                , size = newSize
            }


get : k -> Dict k v -> Maybe v
get key (Dict dict) =
    let
        index =
            bucketIndex (dict.hash key) dict.bucketsShift
    in
    -- TODO PERF: perhaps inline these Maybe functions?
    Array.get index dict.buckets
        |> Maybe.andThen (List.find (\( k, _ ) -> k == key))
        |> Maybe.map Tuple.second


resize : Inner k v -> Inner k v
resize dict =
    if dict.usedBuckets > Bitwise.shiftRightZfBy fillThresholdShift dict.bucketsCapacity * fillThresholdMul then
        let
            newShift =
                dict.bucketsShift - 1

            newCapacity =
                2 ^ (32 - newShift)

            emptyNewDict =
                { dict
                    | buckets = Array.repeat newCapacity []
                    , usedBuckets = 0
                    , bucketsCapacity = newCapacity
                    , bucketsShift = newShift
                    , size = 0
                }
        in
        dict.buckets
            |> Array.toList
            |> List.concat
            |> List.foldl addEntry emptyNewDict

    else
        dict


insert : k -> v -> Dict k v -> Dict k v
insert key value (Dict dict) =
    dict
        |> addEntry ( key, value )
        |> resize
        |> Dict


fromList : (k -> Int) -> List ( k, v ) -> Dict k v
fromList hash list =
    List.foldl addEntry (emptyInner hash) list
        |> resize
        |> Dict
