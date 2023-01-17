module Hash.FNV1a exposing
    ( mix, initialSeed
    , charWithSeed, string
    )

{-| Copied from robinheghan/fnv1a @ 1.0.0 and modified the API.

@docs mix, initialSeed
@docs charWithSeed, string

-}

import Bitwise as Bit


initialSeed : Int
initialSeed =
    0x811C9DC5


string : String -> Int
string str =
    stringWithSeed str initialSeed


stringWithSeed : String -> Int -> Int
stringWithSeed str seed =
    Bit.shiftRightZfBy 0 (String.foldl charWithSeed seed str)


charWithSeed : Char -> Int -> Int
charWithSeed char acc =
    {- Implementation copied from: https://github.com/zwilias/elm-utf-tools/tree/2.0.1 -}
    let
        byte =
            Char.toCode char
    in
    if byte < 0x80 then
        mix byte acc

    else if byte < 0x0800 then
        acc
            |> mix (Bit.or 0xC0 <| Bit.shiftRightZfBy 6 byte)
            |> mix (Bit.or 0x80 <| Bit.and 0x3F byte)

    else if byte < 0x00010000 then
        acc
            |> mix (Bit.or 0xE0 <| Bit.shiftRightZfBy 12 byte)
            |> mix (Bit.or 0x80 <| Bit.and 0x3F <| Bit.shiftRightZfBy 6 byte)
            |> mix (Bit.or 0x80 <| Bit.and 0x3F byte)

    else
        acc
            |> mix (Bit.or 0xF0 <| Bit.shiftRightZfBy 18 byte)
            |> mix (Bit.or 0x80 <| Bit.and 0x3F <| Bit.shiftRightZfBy 12 byte)
            |> mix (Bit.or 0x80 <| Bit.and 0x3F <| Bit.shiftRightZfBy 6 byte)
            |> mix (Bit.or 0x80 <| Bit.and 0x3F byte)


mix : Int -> Int -> Int
mix byte hashValue =
    {- Implementation ported from: https://gist.github.com/vaiorabbit/5657561 -}
    let
        mixed =
            Bit.xor byte hashValue
    in
    mixed
        + Bit.shiftLeftBy 1 mixed
        + Bit.shiftLeftBy 4 mixed
        + Bit.shiftLeftBy 7 mixed
        + Bit.shiftLeftBy 8 mixed
        + Bit.shiftLeftBy 24 mixed
