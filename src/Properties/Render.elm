module Properties.Render exposing (propertiesPallet)

--external modules

import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Color exposing (Color)
import Color.Convert as CC


--internal modules

import Properties exposing (..)
import Messages.ClickTarget
import Events
import Messages exposing (Msg)


{- }
   symbols =
       [ --Property Pallet Symbols
       , Svg.symbol [ SvgA.id "strokeColorWidget", SvgA.viewBox "0 0 150 100", SvgA.preserveAspectRatio "none" ]
           [ Svg.rect
               [ SvgA.fill "#ffffff"
               , SvgA.stroke "none"
               , SvgA.x "0"
               , SvgA.y "0"
               , SvgA.width "150"
               , SvgA.height "100"
               ]
               []
           , Svg.text_ [SvgA.x "75", SvgA.y widgetLabelSize, SvgA.fontSize widgetLabelSize, SvgA.textAnchor "middle"]
               [Svg.text "Stroke"]
           ]
       ]
-}


propertiesPallet : Int -> Int -> Float -> PropertyPalletState -> Svg Msg
propertiesPallet x y height palletState =
    --All widgets have a width 50% larger then their height
    let
        widgetList =
            [ StrokeColorPicker, FillColorPicker ]

        widgetHeight =
            List.length widgetList
                |> toFloat
                |> (/) (height - borderSize)
                |> flip (-) borderSize

        widgetWidth =
            widgetHeight * 1.5

        width =
            widgetWidth + borderSize * 2

        borderSize =
            5

        labelFontSize =
            18

        labelFontColor =
            "#000000"

        widgetX =
            toFloat <| x + borderSize

        positionedWidgets =
            positionWidgets widgetList 0

        positionWidgets widgetList index =
            case widgetList of
                [] ->
                    []

                w :: ws ->
                    (getWidgetFunction w) index :: positionWidgets ws (index + 1)

        getWidgetFunction : PropertyWidget -> (Int -> Svg Msg)
        getWidgetFunction widget =
            case widget of
                FillColorPicker ->
                    fillColorWidget

                StrokeColorPicker ->
                    strokeColorWidget

                StrokeWidth ->
                    Debug.crash "TODO implement Stroke Width widget"

                PropertyPalletHandle ->
                    (\_ -> Svg.g [] [])

        yPosFromIndex index =
            toFloat index * (widgetHeight + borderSize) + toFloat y + borderSize

        strokeColorWidget : Int -> Svg Msg
        strokeColorWidget index =
            Svg.g []
                [ widgetBackgroundBox index
                , label "Stroke" index
                , colorBox index palletState.strokeColor
                ]

        fillColorWidget : Int -> Svg Msg
        fillColorWidget index =
            Svg.g []
                [ widgetBackgroundBox index
                , label "Fill" index
                , colorBox index palletState.fillColor
                ]

        label labelText index =
            Svg.text_
                [ SvgA.x <| toString (widgetX + widgetWidth / 2)
                , SvgA.y <| toString (yPosFromIndex index + labelFontSize)
                , SvgA.fontSize <| toString labelFontSize
                , SvgA.textAnchor "middle"
                ]
                [ Svg.text labelText ]

        colorBox : Int -> Color -> Svg Msg
        colorBox index color =
            Svg.rect
                [ SvgA.x <| toString (widgetX + widgetWidth * 0.1)
                , SvgA.width <| toString (widgetWidth * 0.8)
                , SvgA.y <| toString (yPosFromIndex index + labelFontSize + borderSize)
                , SvgA.height <| toString (widgetHeight - labelFontSize - borderSize * 2)
                , SvgA.fill <| CC.colorToHex color
                , SvgA.stroke "#000000"
                , SvgA.strokeWidth <| toString (borderSize / 2)
                ]
                []

        widgetBackgroundBox : Int -> Svg Msg
        widgetBackgroundBox index =
            Svg.rect
                [ SvgA.fill "#ffffff"
                , SvgA.stroke "none"
                , SvgA.x <| toString widgetX
                , SvgA.y <| toString (yPosFromIndex index)
                , SvgA.width <| toString widgetWidth
                , SvgA.height <| toString widgetHeight
                ]
                []
    in
        Svg.g [] <|
            [ Svg.rect
                --background
                [ SvgA.x <| toString x
                , SvgA.y <| toString y
                , SvgA.width <| toString width
                , SvgA.height <| toString height
                , SvgA.fill "#0254d8"
                , Events.mouseUpWithClickTarget <| Messages.ClickTarget.PropertiesPallet PropertyPalletHandle
                , Events.mouseDownWithClickTarget <| Messages.ClickTarget.PropertiesPallet PropertyPalletHandle
                ]
                []
            ]
                ++ positionedWidgets
