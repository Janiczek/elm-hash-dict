module Hash exposing
    ( hashBytes, hashBytesWithSeed
    , int, char, string
    )

{-| A combinator library for hashing arbitrary values into 32bit Ints.

Internally uses FNV1a.

@docs hashBytes, hashBytesWithSeed
@docs int, char, string

-}

import Bitwise
import Hash.FNV1a


{-| TODO document that this really needs bytes (uint8)
TODO is that true? does it misbehave with larger ones?
-}
hashBytes : List Int -> Int
hashBytes bytes =
    hashBytesWithSeed bytes Hash.FNV1a.initialSeed


hashBytesWithSeed : List Int -> Int -> Int
hashBytesWithSeed bytes seed =
    List.foldl Hash.FNV1a.mix seed bytes



--


string : String -> Int
string =
    Hash.FNV1a.string


int : Int -> Int
int n =
    -- TODO this doesn't seem very uniform when trying out:
    -- List.map H.int (List.range 0 10)
    -- > [2615243109,2598465490,2581687871,2564910252,2548132633,2531355014,2514577395,2497799776,2749464061,2732686442,2715908823]
    hashBytes (int64ToBytes n)


char : Char -> Int
char c =
    Bitwise.shiftRightZfBy 0 (Hash.FNV1a.charWithSeed c Hash.FNV1a.initialSeed)



--


{-| Try to get as many bytes from the Int as JS will let us.
-}
int64ToBytes : Int -> List Int
int64ToBytes n =
    [ (n // 0x0000000100000000) |> Bitwise.shiftRightZfBy 24 |> Bitwise.and 0xFF
    , (n // 0x0000000100000000) |> Bitwise.shiftRightZfBy 16 |> Bitwise.and 0xFF
    , (n // 0x0000000100000000) |> Bitwise.shiftRightZfBy 8 |> Bitwise.and 0xFF
    , (n // 0x0000000100000000) |> Bitwise.and 0xFF
    , n |> Bitwise.shiftRightZfBy 24 |> Bitwise.and 0xFF
    , n |> Bitwise.shiftRightZfBy 16 |> Bitwise.and 0xFF
    , n |> Bitwise.shiftRightZfBy 8 |> Bitwise.and 0xFF
    , n |> Bitwise.and 0xFF
    ]
