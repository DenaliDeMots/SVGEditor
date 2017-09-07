module Utilities exposing (until, maybeToList, lastInList, intPositionToFloat, distance, getOffset, applyOffset)

import Task exposing (Task)


until predicate update state =
    if predicate state then
        state
    else
        until predicate update (update state)


maybeToList : Maybe a -> List a
maybeToList a =
    case a of
        Nothing ->
            []

        Just x ->
            [ x ]


lastInList : List a -> List a
lastInList list =
    case list of
        [] ->
            []

        x :: [] ->
            [ x ]

        x :: xs ->
            lastInList xs


intPositionToFloat : { x : Int, y : Int } -> { x : Float, y : Float }
intPositionToFloat pos =
    { x = toFloat pos.x, y = toFloat pos.y }


distance : { x : Float, y : Float } -> { x : Float, y : Float } -> Float
distance start end =
    sqrt <|
        (abs <| start.x - end.x)
            ^ 2
            + (abs <| start.y - end.y)
            ^ 2


getOffset : { x : Int, y : Int } -> { x : Int, y : Int } -> { x : Int, y : Int }
getOffset position1 position2 =
    { x = position2.x - position1.x, y = position2.y - position1.y }


applyOffset : { x : Int, y : Int } -> { x : Int, y : Int } -> { x : Int, y : Int }
applyOffset offset position =
    { x = position.x + offset.x, y = position.y + offset.y }



-- applyOffset (getOffset pos1 pos2)  pos1 == pos2
