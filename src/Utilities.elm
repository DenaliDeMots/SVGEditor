module Utilities exposing (until, maybeToList, lastInList, intPositionToFloat, distance)

import Task exposing (Task)


until predicate update state =
    if predicate state then
        state
    else
        until predicate update (update state)


until_A : (state -> Bool) -> (state -> state) -> state -> Task Never state
until_A predicate update state =
    if predicate (Debug.log "state" state) then
        Task.succeed state
    else
        Task.andThen (until_A predicate update) (Task.succeed <| update state)


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
