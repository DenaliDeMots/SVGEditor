module Events exposing (mouseDownWithClickTarget, mouseUpWithClickTarget, mouseOutWithClickTarget)

import Html.Events
import Json.Decode
import Mouse
import Messages as Msg


mouseDownWithClickTarget clickTarget =
    onMouseDownIsolated (Msg.MouseDown clickTarget)


mouseUpWithClickTarget clickTarget =
    onMouseUpIsolated (Msg.MouseUp clickTarget)


mouseOutWithClickTarget clickTarget =
    onMouseOutIsolated (Msg.MouseOut clickTarget)


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


onMouseOutIsolated tagger =
    Html.Events.onWithOptions
        "mouseout"
        { stopPropagation = True, preventDefault = True }
        (Json.Decode.succeed tagger)
