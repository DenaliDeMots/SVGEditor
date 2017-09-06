module Messages.ClickTarget exposing (..)

import Graphic
import Tool exposing (Tool)
import Properties exposing (PropertyWidget)
import Pallet exposing (Pallet)


type ClickTarget
    = Graphic Graphic.Graphic
    | Screen
    | ToolPallet Tool
    | ToolPalletHandle Pallet
    | PropertiesPallet PropertyWidget
