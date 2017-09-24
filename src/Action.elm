module Action exposing (..)

--This module describes the action states that a user might be performing

import Pallet exposing (Pallet)


type Action
    = None
    | Draw DrawAction
    | SelectFillColor PickerSelectAction
    | SelectStrokeColor PickerSelectAction
    | MovePallet Pallet Offset


type DrawAction
    = DrawRect Position
    | DrawElipse Position
    | DrawPolygon { x : Float, y : Float } (List { x : Float, y : Float })


type PickerSelectAction
    = DraggingSatLight --Dragging mouse on the saturation/lightness box
    | DraggingHue --Dragging the mouse on the hue slider
    | NotDragging --The colorpicker is open but nothing is being dragged


type alias Position =
    { x : Int, y : Int }


type alias Offset =
    { x : Int, y : Int }
