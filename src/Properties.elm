module Properties exposing (PropertyWidget(..), PropertyPalletState, toCommonProperties)

import Color.Convert as CC
import Graphic
import Color exposing (Color)


type PropertyWidget
    = FillColorPicker
    | StrokeColorPicker
    | StrokeWidth


type alias PropertyPalletState =
    { fillColor : Color
    , strokeColor : Color
    , strokeWidth : Float
    }


toCommonProperties : PropertyPalletState -> Graphic.CommonAttributes
toCommonProperties properties =
    { stroke = CC.colorToHex properties.strokeColor
    , strokeWidth = toString properties.strokeWidth
    , fill = CC.colorToHex properties.fillColor
    }
