module HashDict01Naive exposing
    ( Dict
    , empty, singleton, insert, update, remove
    , isEmpty, member, get, size
    , keys, values, toList, fromList
    , map, fold, filter, partition
    , union, intersect, diff
    )

{-| This is:

01Naive = A base to try different optimizations on.


# Dictionaries

@docs Dict


# Build

@docs empty, singleton, insert, update, remove


# Query

@docs isEmpty, member, get, size


# Lists

@docs keys, values, toList, fromList


# Transform

@docs map, fold, filter, partition


# Combine

@docs union, intersect, diff

-}

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
        index =
            bucketIndex (dict.hash key) dict.bucketsShift

        bucket =
            Array.get index dict.buckets
                |> unwrapOrLoop

        -- can't happen if we have our bucket bitwise logic correct
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

        ( newBucket, newSize ) =
            insertLoop [] bucket

        newUsedBuckets =
            if List.isEmpty bucket then
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


size : Dict k v -> Int
size (Dict dict) =
    dict.size


isEmpty : Dict k v -> Bool
isEmpty (Dict dict) =
    dict.size == 0


member : k -> Dict k v -> Bool
member key (Dict dict) =
    let
        index =
            bucketIndex (dict.hash key) dict.bucketsShift
    in
    Array.get index dict.buckets
        |> Maybe.map (List.any (\( k, _ ) -> k == key))
        |> Maybe.withDefault False


toList : Dict k v -> List ( k, v )
toList (Dict dict) =
    dict.buckets
        |> Array.toList
        |> List.concat


singleton : (k -> Int) -> k -> v -> Dict k v
singleton hash key value =
    emptyInner hash
        |> addEntry ( key, value )
        |> resize
        |> Dict


keys : Dict k v -> List k
keys (Dict dict) =
    dict.buckets
        |> Array.toList
        |> List.concatMap (List.map Tuple.first)


values : Dict k v -> List v
values (Dict dict) =
    dict.buckets
        |> Array.toList
        |> List.concatMap (List.map Tuple.second)


fold : (k -> v -> acc -> acc) -> acc -> Dict k v -> acc
fold fn acc (Dict dict) =
    Array.foldl
        (\bucket accA ->
            List.foldl
                (\( k, v ) accL -> fn k v accL)
                accA
                bucket
        )
        acc
        dict.buckets


remove : k -> Dict k v -> Dict k v
remove key ((Dict dict) as wrappedDict) =
    let
        index =
            bucketIndex (dict.hash key) dict.bucketsShift
    in
    case Array.get index dict.buckets of
        Nothing ->
            wrappedDict

        Just bucket ->
            let
                removeLoop : List ( k, v ) -> List ( k, v ) -> Maybe (List ( k, v ))
                removeLoop acc remaining =
                    case remaining of
                        [] ->
                            Nothing

                        (( xk, _ ) as x) :: xs ->
                            if xk == key then
                                Just (acc ++ xs)

                            else
                                removeLoop (x :: acc) xs
            in
            case removeLoop [] bucket of
                Nothing ->
                    wrappedDict

                Just newBucket ->
                    Dict
                        { dict
                            | buckets = Array.set index newBucket dict.buckets
                            , size = dict.size - 1
                        }


update : k -> (Maybe v -> Maybe v) -> Dict k v -> Dict k v
update key fn ((Dict dict) as wrappedDict) =
    let
        index =
            bucketIndex (dict.hash key) dict.bucketsShift

        onNothing () =
            case fn Nothing of
                Nothing ->
                    wrappedDict

                Just value ->
                    insert key value wrappedDict
    in
    case Array.get index dict.buckets of
        Nothing ->
            onNothing ()

        Just bucket ->
            let
                updateLoop : List ( k, v ) -> List ( k, v ) -> Dict k v
                updateLoop acc remaining =
                    case remaining of
                        [] ->
                            onNothing ()

                        (( xk, xv ) as x) :: xs ->
                            if xk == key then
                                case fn (Just xv) of
                                    Nothing ->
                                        -- remove
                                        let
                                            newBucket =
                                                acc ++ xs
                                        in
                                        Dict
                                            { dict
                                                | size = dict.size - 1
                                                , buckets = Array.set index newBucket dict.buckets
                                            }

                                    Just newValue ->
                                        -- replace
                                        let
                                            newBucket =
                                                (( xk, newValue ) :: acc) ++ xs
                                        in
                                        Dict { dict | buckets = Array.set index newBucket dict.buckets }

                            else
                                updateLoop (x :: acc) xs
            in
            updateLoop [] bucket


map : (k -> v1 -> v2) -> Dict k v1 -> Dict k v2
map fn (Dict dict) =
    let
        newBuckets =
            dict.buckets
                |> Array.map (List.map (\( k, v ) -> ( k, fn k v )))
    in
    Dict
        { buckets = newBuckets
        , hash = dict.hash
        , size = dict.size
        , usedBuckets = dict.usedBuckets
        , bucketsCapacity = dict.bucketsCapacity
        , bucketsShift = dict.bucketsShift
        }


filter : (k -> v -> Bool) -> Dict k v -> Dict k v
filter pred (Dict dict) =
    let
        ( newBuckets, newSize, _ ) =
            Array.foldl
                (\bucket ( accBuckets, accSizeA, i ) ->
                    let
                        ( newBucket, newSize_ ) =
                            List.foldl
                                (\(( k, v ) as x) ( accBucket, accSizeL ) ->
                                    if pred k v then
                                        ( x :: accBucket, accSizeL )

                                    else
                                        ( accBucket, accSizeL - 1 )
                                )
                                ( [], accSizeA )
                                bucket
                    in
                    ( if newSize_ == accSizeA then
                        accBuckets

                      else
                        Array.set i newBucket accBuckets
                    , newSize_
                    , i + 1
                    )
                )
                ( dict.buckets, dict.size, 0 )
                dict.buckets
    in
    Dict
        { dict
            | buckets = newBuckets
            , size = newSize
        }


union : Dict k v -> Dict k v -> Dict k v
union t1 t2 =
    fold insert t2 t1


intersect : Dict k v -> Dict k v -> Dict k v
intersect t1 t2 =
    filter (\k _ -> member k t2) t1


diff : Dict k a -> Dict k b -> Dict k a
diff t1 t2 =
    fold (\k v t -> remove k t) t1 t2


partition : (k -> v -> Bool) -> Dict k v -> ( Dict k v, Dict k v )
partition isGood ((Dict dict) as wrappedDict) =
    let
        add key value ( t1, t2 ) =
            if isGood key value then
                ( insert key value t1, t2 )

            else
                ( t1, insert key value t2 )
    in
    fold add ( empty dict.hash, empty dict.hash ) wrappedDict


unwrapOrLoop : Maybe a -> a
unwrapOrLoop maybe =
    let
        crash : Int -> Int
        crash n =
            1 + crash n
    in
    case maybe of
        Nothing ->
            let
                _ =
                    crash 0
            in
            unwrapOrLoop maybe

        Just a ->
            a
