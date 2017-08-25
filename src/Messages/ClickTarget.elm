module Messages.ClickTarget exposing (..)

import Graphic
import Tool exposing (Tool)


type ClickTarget
    = Graphic Graphic.Graphic
    | Screen
    | ToolPallet Tool
