module Utilities exposing (until, maybeToList)

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
