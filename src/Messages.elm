module Messages exposing (..)

--Exernal modules

import Window
import Mouse


--Internal modules

import Messages.ClickTarget as ClickTarget exposing (ClickTarget)


type Msg
    = Resize Window.Size --Sent by Window subscription
    | MouseDown ClickTarget Mouse.Position --Sent by Svg
    | MouseUp ClickTarget Mouse.Position --Sent by Svg
    | MouseMove Mouse.Position --Sent by Mouse subscription
