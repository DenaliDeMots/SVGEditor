module Action exposing (..)

import Pallet exposing (Pallet)


type Action
    = None
    | Draw DrawAction
    | SelectFillColor
    | SelectStrokeColor
    | MovePallet Pallet Offset


type DrawAction
    = DrawRect Position
    | DrawElipse Position
    | DrawPolygon { x : Float, y : Float } (List { x : Float, y : Float })


type alias Position =
    { x : Int, y : Int }


type alias Offset =
    { x : Int, y : Int }
