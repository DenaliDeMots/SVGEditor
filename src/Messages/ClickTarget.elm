module Messages.ClickTarget exposing (..)

import Graphic
import Tool exposing (Tool)
import Properties exposing (PropertyWidget)


type ClickTarget
    = Graphic Graphic.Graphic
    | Screen
    | ToolPallet Tool
    | ToolPalletHandle
    | PropertiesPallet PropertyWidget
