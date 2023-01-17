module ListExtra exposing (find)

{-|

@docs find

-}


{-| Taken from elm-community/list-extra
-}
find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                find predicate rest
