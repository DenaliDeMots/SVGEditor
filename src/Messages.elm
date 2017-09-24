module Messages exposing (..)

--Defines the Msg type that the view code emits and the update code consumes
--Exernal modules

import Window
import Mouse


--Internal modules

import Messages.ClickTarget as ClickTarget exposing (ClickTarget)
import Messages.UpdatePropertyPallet as PPS


type Msg
    = Resize Window.Size --Sent by Window subscription
    | MouseDown ClickTarget Mouse.Position --Sent by Svg
    | MouseUp ClickTarget Mouse.Position --Sent by Svg
    | MouseMove Mouse.Position --Sent by Mouse subscription
    | UpdatePropertyPalletState PPS.UpdatePropertyPalletState
