module Properties.Render exposing (propertiesPallet)

--This module defines the propertiesPallet function which renders the Property Pallet as Svg
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
import Action exposing (Action)
import Messages exposing (Msg)
import Pallet
import Utilities


propertiesPallet : Int -> Int -> Float -> PropertyPalletState -> Action -> Svg Msg
propertiesPallet x y height palletState currentAction =
    --All widgets have a width 50% larger then their height
    let
        widgetList =
            --Defines which controls appear in the Property Pallet and their order
            [ StrokeColor ColorBox, FillColor ColorBox, StrokeWidth Properties.Increment ]

        borderSize =
            5

        labelFontSize =
            18

        labelFontColor =
            "#000000"

        --Derived values
        widgetHeight =
            List.length widgetList
                |> toFloat
                |> (/) (height - borderSize)
                |> flip (-) borderSize

        widgetWidth =
            widgetHeight * 1.5

        width =
            widgetWidth + borderSize * 2

        widgetX =
            toFloat <| x + borderSize

        positionedWidgets =
            --Convert the widgetList into a list of Svg Msg data
            positionWidgets widgetList 0

        positionWidgets widgetList index =
            --calls the widget helper functions with the index of the widget in the widgetList
            case widgetList of
                [] ->
                    []

                w :: ws ->
                    (getWidgetFunction w) index :: positionWidgets ws (index + 1)

        getWidgetFunction : PropertyWidget -> (Int -> Svg Msg)
        getWidgetFunction widget =
            --maps the widgets in the widgetList to their corrosponding helper functions
            case widget of
                FillColor _ ->
                    fillColorWidget

                StrokeColor _ ->
                    strokeColorWidget

                StrokeWidth _ ->
                    strokeWidthWidget

        yPosFromIndex index =
            toFloat index * (widgetHeight + borderSize) + toFloat y + borderSize

        positionedColorPicker =
            --Create Svg Msg data representing the color picker
            case currentAction of
                Action.SelectFillColor pickerAction ->
                    colorPicker (Utilities.indexOf (FillColor ColorBox) widgetList |> abs) palletState.fillColor Fill

                Action.SelectStrokeColor pickerAction ->
                    colorPicker (Utilities.indexOf (StrokeColor ColorBox) widgetList |> abs) palletState.strokeColor Stroke

                _ ->
                    Svg.g [] []

        --widget functions--
        strokeColorWidget : Int -> Svg Msg
        strokeColorWidget index =
            Svg.g []
                [ widgetBackgroundBox index
                , label "Stroke" index
                , colorBox index palletState.strokeColor (StrokeColor ColorBox)
                ]

        fillColorWidget : Int -> Svg Msg
        fillColorWidget index =
            Svg.g []
                [ widgetBackgroundBox index
                , label "Fill" index
                , colorBox index palletState.fillColor (FillColor ColorBox)
                ]

        strokeWidthWidget : Int -> Svg Msg
        strokeWidthWidget index =
            Svg.g []
                [ widgetBackgroundBox index
                , label "Line Size" index
                , numberControls index
                ]

        --helper widget functions--
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
                    Messages.ClickTarget.PropertiesPallet (Properties.StrokeWidth Properties.Increment)
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

        ------------------------------------------------------------------------
        --Color Picker
        ------------------------------------------------------------------------
        colorPicker : Int -> Color -> ColorPickerTarget -> Svg Msg
        colorPicker index color target =
            let
                pickerRectWidth =
                    180

                pickerHeightRatio =
                    1.2

                arrowWidth =
                    40

                padding =
                    10

                --Derived values
                pickerWidth =
                    pickerRectWidth + arrowWidth

                pickerHeight =
                    pickerRectWidth * pickerHeightRatio

                saturationBoxSize =
                    pickerRectWidth - (padding * 2)

                ( hue, hueFloat, saturationFloat, lightnessFloat ) =
                    --Break appart the current color into its compnents
                    let
                        hsl =
                            Color.toHsl color

                        hueFloat =
                            hsl.hue |> Utilities.replaceNaNWith 0

                        saturationFloat =
                            hsl.saturation |> Utilities.replaceNaNWith 1

                        lightnessFloat =
                            hsl.lightness |> Utilities.replaceNaNWith 1
                    in
                        ( Color.hsl hueFloat 1 0.5, hueFloat, saturationFloat, lightnessFloat )

                saturationBoxDimensions =
                    [ SvgA.x <| toString padding
                    , SvgA.y <| toString padding
                    , SvgA.width <| toString saturationBoxSize
                    , SvgA.height <| toString saturationBoxSize
                    ]

                hueBoxHeight =
                    pickerHeight - saturationBoxSize - (padding * 3)

                hueBoxDimensions =
                    [ SvgA.x <| toString padding
                    , SvgA.y <| toString (saturationBoxSize + padding * 2)
                    , SvgA.height <| toString hueBoxHeight
                    , SvgA.width <| toString saturationBoxSize
                    ]

                pickerOrigin =
                    { x = widgetX - pickerWidth
                    , y =
                        --Center arrow on middle of color box
                        (yPosFromIndex index
                            + labelFontSize
                            + borderSize
                            + (widgetHeight - labelFontSize - borderSize * 2)
                            / 2
                         --calculates the y value of the middle of the color box
                        )
                            - pickerHeight
                            / 2
                    }

                saturationBoxOrigin =
                    { x = pickerOrigin.x + padding, y = pickerOrigin.y + padding }

                hueBoxOrigin =
                    { x = pickerOrigin.x + padding, y = pickerOrigin.y + saturationBoxSize + padding * 2 }

                selectSatLight : { x : Int, y : Int } -> Color
                selectSatLight pos =
                    --Conversion function that gets bundled into the Messages that the color picker emits
                    --This function captures the relative position information of the Saturation/Lightness box
                    --so that the main update function can use the absolute position of a mouse click to calculate
                    --the new color
                    let
                        relativeX =
                            toFloat pos.x - saturationBoxOrigin.x

                        relativeY =
                            toFloat pos.y - saturationBoxOrigin.y
                    in
                        Color.hsl hueFloat
                            (relativeY / saturationBoxSize)
                            ((saturationBoxSize - relativeX) / saturationBoxSize)

                selectHue : { x : Int, y : Int } -> Color
                selectHue pos =
                    --Conversion function that gets bundled into the Messages that the color picker emits
                    --This function captures the relative position information of the Hue box so that
                    --the main update function can use the absolute position of a mouse click to calculate
                    --the new color
                    let
                        relativeX =
                            toFloat pos.x - hueBoxOrigin.x

                        xPercentage =
                            relativeX / saturationBoxSize

                        offset =
                            0
                    in
                        Color.hsl
                            (degrees <| xPercentage * 360)
                            saturationFloat
                            lightnessFloat

                events =
                    --defines mouseUp and mouseDown events to be used by the color picker components based whether the
                    --color picker is targeted to the fill or stroke color
                    case target of
                        Fill ->
                            { mouseUp =
                                (\pickerWidget ->
                                    Events.mouseUpWithClickTarget <| Messages.ClickTarget.PropertiesPallet <| FillColor pickerWidget
                                )
                            , mouseDown =
                                (\pickerWidget ->
                                    Events.mouseDownWithClickTarget <| Messages.ClickTarget.PropertiesPallet <| FillColor pickerWidget
                                )
                            }

                        Stroke ->
                            { mouseUp =
                                (\pickerWidget ->
                                    Events.mouseUpWithClickTarget <| Messages.ClickTarget.PropertiesPallet <| StrokeColor pickerWidget
                                )
                            , mouseDown =
                                (\pickerWidget ->
                                    Events.mouseDownWithClickTarget <| Messages.ClickTarget.PropertiesPallet <| StrokeColor pickerWidget
                                )
                            }
            in
                Svg.svg
                    [ SvgA.x <| toString pickerOrigin.x
                    , SvgA.y <| toString pickerOrigin.y
                    , SvgA.width <| toString pickerWidth
                    , SvgA.height <| toString pickerHeight
                    , SvgA.viewBox <| "0 0 " ++ toString (pickerWidth) ++ " " ++ toString pickerHeight
                    ]
                    [ Svg.defs
                        --Defines the gradients used in the color picker
                        []
                        [ Svg.linearGradient
                            --Goes from transparent at the bottom to middle grey at the top
                            [ SvgA.id "saturation"
                            , SvgA.x1 "0"
                            , SvgA.y1 "1"
                            , SvgA.x2 "0"
                            , SvgA.y2 "0"
                            ]
                            [ Svg.stop [ SvgA.offset "0", SvgA.stopColor "#808080", SvgA.stopOpacity "0" ] []
                            , Svg.stop [ SvgA.offset "1", SvgA.stopColor "#808080", SvgA.stopOpacity "1" ] []
                            ]
                        , Svg.linearGradient [ SvgA.id "lightness" ]
                            --Goes from white at the left to transparent in the center to black at the right
                            [ Svg.stop [ SvgA.offset "0", SvgA.stopColor "#fff", SvgA.stopOpacity "1" ] []
                            , Svg.stop [ SvgA.offset "0.499", SvgA.stopColor "#fff", SvgA.stopOpacity "0" ] []
                            , Svg.stop [ SvgA.offset "0.5", SvgA.stopColor "#000", SvgA.stopOpacity "0" ] []
                            , Svg.stop [ SvgA.offset "1", SvgA.stopColor "#000", SvgA.stopOpacity "1" ] []
                            ]
                        , Svg.linearGradient
                            --Creates a rainbow from left to right
                            [ SvgA.id "hue" ]
                            [ Svg.stop [ SvgA.offset "0", SvgA.stopColor "#FF0000" ] []
                            , Svg.stop [ SvgA.offset "0.17", SvgA.stopColor "#FFFF00" ] []
                            , Svg.stop [ SvgA.offset "0.33", SvgA.stopColor "#00FF00" ] []
                            , Svg.stop [ SvgA.offset "0.5", SvgA.stopColor "#00FFFF" ] []
                            , Svg.stop [ SvgA.offset "0.66", SvgA.stopColor "#0000FF" ] []
                            , Svg.stop [ SvgA.offset "0.83", SvgA.stopColor "#FF00FF" ] []
                            , Svg.stop [ SvgA.offset "1", SvgA.stopColor "#FF0000" ] []
                            ]
                        ]
                    , Svg.polygon
                        --outer arrow box
                        [ SvgA.fill "#ededed"
                        , events.mouseUp Background
                        , events.mouseDown Background
                        , SvgA.points <|
                            "0 0, "
                                ++ (toString pickerRectWidth ++ " 0, ")
                                ++ (toString (pickerWidth) ++ " " ++ toString (pickerHeight / 2) ++ ", ")
                                ++ (toString pickerRectWidth ++ " " ++ toString pickerHeight ++ ", ")
                                ++ ("0 " ++ toString pickerHeight)
                        ]
                        []
                    , Svg.rect
                        --SaturationBox Hue
                        ([ SvgA.fill (CC.colorToHex hue) ] ++ saturationBoxDimensions)
                        []
                    , Svg.rect
                        --saturation level overlay
                        ([ SvgA.fill "url(#saturation)" ] ++ saturationBoxDimensions)
                        []
                    , Svg.rect
                        --lightness overlay
                        ([ SvgA.fill "url(#lightness)"
                         , SvgA.style "cursor: crosshair"
                         , events.mouseUp <| SaturationLightness selectSatLight
                         , events.mouseDown <| SaturationLightness selectSatLight
                         ]
                            ++ saturationBoxDimensions
                        )
                        []
                    , Svg.circle
                        --Saturation/lightness indicator
                        [ SvgA.cx <| toString <| padding + ((1 - lightnessFloat) * saturationBoxSize)
                        , SvgA.cy <| toString <| padding + (saturationFloat * saturationBoxSize)
                        , SvgA.r "3"
                        , SvgA.fill "none"
                        , SvgA.stroke "#808080"
                        , SvgA.strokeWidth "3"
                        ]
                        []
                    , Svg.rect
                        -- Hue picker
                        (hueBoxDimensions
                            ++ [ SvgA.fill "url(#hue)"
                               , events.mouseUp <| Hue selectHue
                               , events.mouseDown <| Hue selectHue
                               ]
                        )
                        []
                    , Svg.rect
                        --Hue indicatior
                        (let
                            thickness =
                                4
                         in
                            [ SvgA.x <| toString <| padding + (hueFloat / (2 * pi)) * saturationBoxSize
                            , SvgA.y <| toString <| saturationBoxSize - (thickness / 2) + padding * 2
                            , SvgA.width "10"
                            , SvgA.height <| toString <| hueBoxHeight + thickness
                            , SvgA.fill "none"
                            , SvgA.stroke "#808080"
                            , SvgA.strokeWidth <| toString thickness
                            ]
                        )
                        []
                    ]
    in
        Svg.g
            --Defines the background and gives it the PalletHandle click target for pallet dragging
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



--helper data type for deciding whether the color picker is selecting the Stroke or Fill color


type ColorPickerTarget
    = Stroke
    | Fill
