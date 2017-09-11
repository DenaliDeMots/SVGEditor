module Properties exposing (PropertyWidget(..), Button(..), PropertyPalletState, toCommonProperties)

import Color.Convert as CC
import ColorPicker
import Graphic
import Color exposing (Color)
import ColorPickerState exposing (ColorPickerState)


type PropertyWidget
    = FillColorPicker
    | StrokeColorPicker
    | StrokeWidth Button


type Button
    = Increment
    | Decrement


type alias PropertyPalletState =
    { fillColor : Color
    , strokeColor : Color
    , strokeWidth : Float
    , colorPickerState : ColorPickerState
    }


toCommonProperties : PropertyPalletState -> Graphic.CommonAttributes
toCommonProperties properties =
    { stroke = CC.colorToHex properties.strokeColor
    , strokeWidth = toString properties.strokeWidth
    , fill = CC.colorToHex properties.fillColor
    }
