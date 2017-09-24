module Events exposing (mouseDownWithClickTarget, mouseUpWithClickTarget)

--This module defines functions that generate Messages

import Html.Events
import Json.Decode
import Mouse
import Messages as Msg


--The click events are tagged with extra ClickTarget data


mouseDownWithClickTarget clickTarget =
    onMouseDownIsolated (Msg.MouseDown clickTarget)


mouseUpWithClickTarget clickTarget =
    onMouseUpIsolated (Msg.MouseUp clickTarget)



--In order to avoid extra complexity in my event system I have all the events
--prevent the default action and stop the event propagation


onMouseDownIsolated tagger =
    Html.Events.onWithOptions
        "mousedown"
        { stopPropagation = True, preventDefault = True }
        (Json.Decode.map tagger Mouse.position)


onMouseUpIsolated tagger =
    Html.Events.onWithOptions
        "mouseup"
        { stopPropagation = True, preventDefault = True }
        (Json.Decode.map tagger Mouse.position)
