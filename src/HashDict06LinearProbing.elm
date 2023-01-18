module HashDict06LinearProbing exposing
    ( Dict
    , empty, singleton, insert, update, remove
    , isEmpty, member, get, size
    , keys, values, toList, fromList
    , map, fold, filter, partition
    , union, intersect, diff
    )

{-| This is:

04WithoutClamp + Linear Probing


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
    , buckets : Array (Bucket k v)
    , size : Int -- actual keys stored
    , sizeWithTombstones : Int -- for resizing
    , capacity : Int -- derivable from shift, but let's cache it. Used in every resize check (after insert)
    , shift : Int
    }


type Bucket k v
    = Empty
    | Entry k v
    | Tombstone


mapBucket : (k -> v1 -> v2) -> Bucket k v1 -> Bucket k v2
mapBucket fn bucket =
    case bucket of
        Entry k v ->
            Entry k (fn k v)

        Empty ->
            Empty

        Tombstone ->
            Tombstone


initShift : Int
initShift =
    -- TODO PERF: measure the best value?
    -- --> shift by this amount == modulo initBucketsCount
    -- example: >>> 30 == % 4 if the hash is 32bit
    30


initCapacity : Int
initCapacity =
    -- if shift == 30, count will be 4
    2 ^ (32 - initShift)


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
    { hash = hash
    , buckets = Array.repeat initCapacity Empty
    , size = 0
    , sizeWithTombstones = 0
    , capacity = initCapacity
    , shift = initShift
    }


bucketIndex : Int -> Int -> Int
bucketIndex hash shift =
    Bitwise.shiftRightZfBy shift hash


addEntry_ : k -> v -> Inner k v -> Inner k v
addEntry_ key value dict =
    addEntry ( key, value ) dict


addEntry : ( k, v ) -> Inner k v -> Inner k v
addEntry ( key, value ) dict =
    let
        initIndex =
            bucketIndex (dict.hash key) dict.shift

        insertLoop : Int -> Inner k v
        insertLoop index =
            if index == dict.capacity then
                insertLoop 0

            else
                case Array.get index dict.buckets |> unwrapOrCrash of
                    Empty ->
                        { dict
                            | buckets = Array.set index (Entry key value) dict.buckets
                            , size = dict.size + 1
                            , sizeWithTombstones = dict.sizeWithTombstones + 1
                        }
                            |> resize

                    Tombstone ->
                        insertLoop (index + 1)

                    Entry k v ->
                        if k == key then
                            { dict | buckets = Array.set index (Entry key value) dict.buckets }

                        else
                            insertLoop (index + 1)
    in
    insertLoop initIndex


get : k -> Dict k v -> Maybe v
get key (Dict dict) =
    let
        initIndex =
            bucketIndex (dict.hash key) dict.shift

        getLoop : Int -> Maybe v
        getLoop index =
            if index == dict.capacity then
                getLoop 0

            else
                case Array.get index dict.buckets |> unwrapOrCrash of
                    Empty ->
                        Nothing

                    Tombstone ->
                        getLoop (index + 1)

                    Entry k v ->
                        if k == key then
                            Just v

                        else
                            getLoop (index + 1)
    in
    getLoop initIndex


resize : Inner k v -> Inner k v
resize dict =
    if dict.sizeWithTombstones > Bitwise.shiftRightZfBy fillThresholdShift dict.capacity * fillThresholdMul then
        let
            newShift =
                dict.shift - 1

            newCapacity =
                2 ^ (32 - newShift)

            emptyNewDict =
                { dict
                    | buckets = Array.repeat newCapacity Empty
                    , capacity = newCapacity
                    , shift = newShift
                    , size = 0
                    , sizeWithTombstones = 0
                }
        in
        fold_ addEntry_ emptyNewDict dict

    else
        dict


insert : k -> v -> Dict k v -> Dict k v
insert key value (Dict dict) =
    dict
        |> addEntry_ key value
        |> Dict


fromList : (k -> Int) -> List ( k, v ) -> Dict k v
fromList hash list =
    List.foldl addEntry (emptyInner hash) list
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
        initIndex =
            bucketIndex (dict.hash key) dict.shift

        memberLoop : Int -> Bool
        memberLoop index =
            if index == dict.capacity then
                memberLoop 0

            else
                case Array.get index dict.buckets |> unwrapOrCrash of
                    Empty ->
                        False

                    Tombstone ->
                        memberLoop (index + 1)

                    Entry k v ->
                        if k == key then
                            True

                        else
                            memberLoop (index + 1)
    in
    memberLoop initIndex


toList : Dict k v -> List ( k, v )
toList (Dict dict) =
    dict.buckets
        |> Array.toList
        |> List.filterMap
            (\entry ->
                case entry of
                    Entry k v ->
                        Just ( k, v )

                    _ ->
                        Nothing
            )


singleton : (k -> Int) -> k -> v -> Dict k v
singleton hash key value =
    emptyInner hash
        |> addEntry ( key, value )
        |> Dict


keys : Dict k v -> List k
keys (Dict dict) =
    dict.buckets
        |> Array.toList
        |> List.filterMap
            (\entry ->
                case entry of
                    Entry k v ->
                        Just k

                    _ ->
                        Nothing
            )


values : Dict k v -> List v
values (Dict dict) =
    dict.buckets
        |> Array.toList
        |> List.filterMap
            (\entry ->
                case entry of
                    Entry k v ->
                        Just v

                    _ ->
                        Nothing
            )


fold : (k -> v -> acc -> acc) -> acc -> Dict k v -> acc
fold fn acc (Dict dict) =
    fold_ fn acc dict


fold_ : (k -> v -> acc -> acc) -> acc -> Inner k v -> acc
fold_ fn acc dict =
    Array.foldl
        (\bucket accA ->
            case bucket of
                Entry k v ->
                    fn k v accA

                _ ->
                    accA
        )
        acc
        dict.buckets


remove : k -> Dict k v -> Dict k v
remove key ((Dict dict) as wrappedDict) =
    let
        initIndex =
            bucketIndex (dict.hash key) dict.shift

        removeLoop : Int -> Dict k v
        removeLoop index =
            if index == dict.capacity then
                removeLoop 0

            else
                case Array.get index dict.buckets |> unwrapOrCrash of
                    Empty ->
                        wrappedDict

                    Tombstone ->
                        removeLoop (index + 1)

                    Entry k v ->
                        if k == key then
                            Dict
                                { dict
                                    | buckets = Array.set index Tombstone dict.buckets
                                    , size = dict.size - 1
                                }

                        else
                            removeLoop (index + 1)
    in
    removeLoop initIndex


update : k -> (Maybe v -> Maybe v) -> Dict k v -> Dict k v
update key fn ((Dict dict) as wrappedDict) =
    let
        initIndex =
            bucketIndex (dict.hash key) dict.shift

        updateLoop : Int -> Dict k v
        updateLoop index =
            if index == dict.capacity then
                updateLoop 0

            else
                case Array.get index dict.buckets |> unwrapOrCrash of
                    Empty ->
                        case fn Nothing of
                            Nothing ->
                                wrappedDict

                            Just value ->
                                -- TODO PERF: you already know the index - do the thing here instead of probing again
                                insert key value wrappedDict

                    Tombstone ->
                        updateLoop (index + 1)

                    Entry k v ->
                        if k == key then
                            case fn (Just v) of
                                Nothing ->
                                    -- TODO PERF: you already know the index - do the thing here instead of probing again
                                    remove key wrappedDict

                                Just newValue ->
                                    -- TODO PERF: you already know the index - do the thing here instead of probing again
                                    insert key newValue wrappedDict

                        else
                            updateLoop (index + 1)
    in
    updateLoop initIndex


map : (k -> v1 -> v2) -> Dict k v1 -> Dict k v2
map fn (Dict dict) =
    Dict
        { buckets = Array.map (mapBucket fn) dict.buckets
        , hash = dict.hash
        , size = dict.size
        , sizeWithTombstones = dict.sizeWithTombstones
        , capacity = dict.capacity
        , shift = dict.shift
        }


filter : (k -> v -> Bool) -> Dict k v -> Dict k v
filter pred (Dict dict) =
    let
        ( newBuckets, newSize, _ ) =
            Array.foldl
                (\bucket ( accBuckets, accSizeA, i ) ->
                    case bucket of
                        Tombstone ->
                            ( accBuckets, accSizeA, i + 1 )

                        Empty ->
                            ( accBuckets, accSizeA, i + 1 )

                        Entry k v ->
                            if pred k v then
                                ( accBuckets, accSizeA, i + 1 )

                            else
                                ( Array.set i Tombstone accBuckets
                                , accSizeA - 1
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


unwrapOrCrash : Maybe a -> a
unwrapOrCrash maybe =
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
            unwrapOrCrash maybe

        Just a ->
            a
