module Messages.ClickTarget exposing (..)

--This module defines the ClickTarget type which is used in messages
--for identifying which component recieved a mousedown or mouseup event

import Graphic
import Tool exposing (Tool)
import Properties exposing (PropertyWidget)
import Pallet exposing (Pallet)


type ClickTarget
    = Graphic Graphic.Graphic
    | Screen
    | ToolPallet Tool
    | PalletHandle Pallet
    | PropertiesPallet PropertyWidget
