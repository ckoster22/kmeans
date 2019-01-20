module NonEmpty exposing (NonEmpty, all, foldl, indexedMap, list, map, takeFromTail, toList)

import Fuzz exposing (Fuzzer, map, tuple)


type alias NonEmpty a =
    ( a, List a )


map : (a -> b) -> NonEmpty a -> NonEmpty b
map mapper ( head, tail ) =
    ( mapper head, List.map mapper tail )


foldl : (a -> b -> b) -> b -> NonEmpty a -> b
foldl callback accumulator ( head, tail ) =
    List.foldl callback accumulator (head :: tail)


indexedMap : (Int -> a -> b) -> NonEmpty a -> NonEmpty b
indexedMap mapper ( head, tail ) =
    ( mapper 0 head, List.indexedMap (\index a -> mapper (index + 1) a) tail )


takeFromTail : Int -> NonEmpty a -> NonEmpty a
takeFromTail numToTake ( head, tail ) =
    ( head, List.take numToTake tail )


all : (a -> Bool) -> NonEmpty a -> Bool
all predicate ( head, tail ) =
    predicate head && List.all predicate tail


toList : NonEmpty a -> List a
toList ( head, tail ) =
    head :: tail


list : Fuzzer a -> Fuzzer (NonEmpty a)
list fuzzer =
    tuple ( fuzzer, Fuzz.list fuzzer )
