module Properties.Render exposing (propertiesPallet)

--external modules

import Svg exposing (Svg)
import Html
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Svg.Attributes as SvgA
import Color exposing (Color)
import Color.Convert as CC


--internal modules

import Properties exposing (..)
import Messages.ClickTarget
import Messages.UpdatePropertyPallet as PPS
import Events
import Messages exposing (Msg)


propertiesPallet : Int -> Int -> Float -> PropertyPalletState -> Svg Msg
propertiesPallet x y height palletState =
    --All widgets have a width 50% larger then their height
    let
        widgetList =
            [ StrokeColorPicker, FillColorPicker, StrokeWidth ]

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
                    strokeWidthWidget

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

        strokeWidthWidget : Int -> Svg Msg
        strokeWidthWidget index =
            Svg.g []
                [ widgetBackgroundBox index
                , label "Line Size" index
                , numberControls index
                ]

        --helper functions
        label labelText index =
            Svg.text_
                [ SvgA.x <| toString (widgetX + widgetWidth / 2)
                , SvgA.y <| toString (yPosFromIndex index + labelFontSize)
                , SvgA.fontSize <| toString labelFontSize
                , SvgA.textAnchor "middle"
                ]
                [ Svg.text labelText ]

        numberControls index =
            Svg.g []
                [ decrementButton index
                , strokeWidthNumber index
                , incrementButton index
                ]

        decrementButton index =
            Svg.svg
                [ SvgA.viewBox "0 0 100 100"
                , SvgA.x <| toString (widgetX + widgetWidth * 0.05)
                , SvgA.y <| toString (yPosFromIndex index + labelFontSize + borderSize)
                , SvgA.width <| toString (widgetWidth * 0.3)
                , SvgA.height <| toString (widgetHeight - labelFontSize - borderSize * 2)
                ]
                [ Svg.polygon
                    [ SvgA.points "0 50, 100 0, 100 100"
                    , SvgA.fill "gold"
                    ]
                    []
                ]

        incrementButton index =
            Svg.svg
                [ SvgA.viewBox "0 0 100 100"
                , SvgA.x <| toString (widgetX + widgetWidth * 0.6)
                , SvgA.y <| toString (yPosFromIndex index + labelFontSize + borderSize)
                , SvgA.width <| toString (widgetWidth * 0.3)
                , SvgA.height <| toString (widgetHeight - labelFontSize - borderSize * 2)
                ]
                [ Svg.polygon
                    [ SvgA.points "0 0, 100 50, 0 100"
                    , SvgA.fill "gold"
                    ]
                    []
                ]

        strokeWidthNumber index =
            Svg.text_
                [ SvgA.x <| toString (widgetX + widgetWidth / 2)
                , SvgA.y <| toString <| yPosFromIndex index + (widgetHeight - borderSize) * 0.92
                , SvgA.fontSize <| toString labelFontSize
                , SvgA.textAnchor "middle"
                ]
                [ Svg.text <| toString palletState.strokeWidth ]

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
