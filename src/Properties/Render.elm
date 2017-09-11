module Properties.Render exposing (propertiesPallet)

--external modules

import Svg exposing (Svg)
import Html
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Svg.Attributes as SvgA
import Color exposing (Color)
import Color.Convert as CC
import ColorPicker


--internal modules

import Properties exposing (..)
import Messages.ClickTarget
import Messages.UpdatePropertyPallet as PPS
import Events
import Messages exposing (Msg)
import Pallet
import Utilities
import ColorPickerState as CPS


propertiesPallet : Int -> Int -> Float -> PropertyPalletState -> Svg Msg
propertiesPallet x y height palletState =
    --All widgets have a width 50% larger then their height
    let
        widgetList =
            [ StrokeColorPicker, FillColorPicker, StrokeWidth Properties.Increment ]

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

                StrokeWidth _ ->
                    strokeWidthWidget

        yPosFromIndex index =
            toFloat index * (widgetHeight + borderSize) + toFloat y + borderSize

        positionedColorPicker =
            case palletState.colorPickerState of
                CPS.SelectingStrokeColor state ->
                    colorPicker (Utilities.indexOf FillColorPicker widgetList |> abs) palletState.fillColor state

                CPS.SelectingFillColor state ->
                    colorPicker (Utilities.indexOf StrokeColorPicker widgetList |> abs) palletState.strokeColor state

                CPS.Hidden ->
                    Svg.g [] []

        --widget functions
        strokeColorWidget : Int -> Svg Msg
        strokeColorWidget index =
            Svg.g []
                [ widgetBackgroundBox index
                , label "Stroke" index
                , colorBox index palletState.strokeColor Properties.StrokeColorPicker
                ]

        fillColorWidget : Int -> Svg Msg
        fillColorWidget index =
            Svg.g []
                [ widgetBackgroundBox index
                , label "Fill" index
                , colorBox index palletState.fillColor Properties.FillColorPicker
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
                , Events.mouseDownWithClickTarget <|
                    Messages.ClickTarget.PropertiesPallet (Properties.StrokeWidth Properties.Decrement)
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
                , Events.mouseDownWithClickTarget <|
                    Messages.ClickTarget.PropertiesPallet (Properties.StrokeWidth Properties.Decrement)
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

        colorBox : Int -> Color -> Properties.PropertyWidget -> Svg Msg
        colorBox index color widget =
            Svg.rect
                [ SvgA.x <| toString (widgetX + widgetWidth * 0.1)
                , SvgA.width <| toString (widgetWidth * 0.8)
                , SvgA.y <| toString (yPosFromIndex index + labelFontSize + borderSize)
                , SvgA.height <| toString (widgetHeight - labelFontSize - borderSize * 2)
                , SvgA.fill <| CC.colorToHex color
                , SvgA.stroke "#000000"
                , SvgA.strokeWidth <| toString (borderSize / 2)
                , Events.mouseDownWithClickTarget <| Messages.ClickTarget.PropertiesPallet widget
                ]
                []

        colorPicker : Int -> Color -> ColorPicker.State -> Svg Msg
        colorPicker index color state =
            let
                pickerHeight =
                    200

                pickerWidth =
                    200
            in
                Svg.foreignObject
                    [ SvgA.x <| toString (widgetX - 20 - pickerWidth)
                    , SvgA.y <| toString <| (yPosFromIndex index + widgetHeight / 2) - pickerHeight / 2
                    , SvgA.width <| toString pickerWidth
                    , SvgA.height <| toString pickerHeight
                    , SvgA.viewBox <| "0 0 " ++ toString pickerWidth ++ " " ++ toString pickerHeight
                    ]
                    [ Html.div []
                        [ ColorPicker.view color state
                            |> Html.map (Messages.UpdatePropertyPalletState << PPS.UpdateColor)
                        ]
                    ]

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
        Svg.g
            [ Events.mouseUpWithClickTarget <| Messages.ClickTarget.PalletHandle Pallet.PropertiesPallet
            , Events.mouseDownWithClickTarget <| Messages.ClickTarget.PalletHandle Pallet.PropertiesPallet
            ]
        <|
            [ Svg.rect
                --background
                [ SvgA.x <| toString x
                , SvgA.y <| toString y
                , SvgA.width <| toString width
                , SvgA.height <| toString height
                , SvgA.fill "#0254d8"
                ]
                []
            ]
                ++ (positionedColorPicker :: positionedWidgets)
