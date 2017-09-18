module Properties exposing (PropertyWidget(..), Button(..), PickerWidget(..), PropertyPalletState, toCommonProperties)

import Color.Convert as CC
import Graphic
import Color exposing (Color)


type PropertyWidget
    = FillColor PickerWidget
    | StrokeColor PickerWidget
    | StrokeWidth Button


type PickerWidget
    = SaturationLightness PositionToColor
    | Hue PositionToColor
    | Background
    | ColorBox


type Button
    = Increment
    | Decrement


type alias PositionToColor =
    { x : Int, y : Int } -> Color


type alias PropertyPalletState =
    { fillColor : Color
    , strokeColor : Color
    , strokeWidth : Float

    --, colorPickerState : ColorPickerState
    }


toCommonProperties : PropertyPalletState -> Graphic.CommonAttributes
toCommonProperties properties =
    { stroke = CC.colorToHex properties.strokeColor
    , strokeWidth = toString properties.strokeWidth
    , fill = CC.colorToHex properties.fillColor
    }
