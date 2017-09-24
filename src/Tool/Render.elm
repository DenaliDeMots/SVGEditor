module Tool.Render exposing (..)

--Defines the toolPallet function which renders the Tool Pallet as Svg
--external modules

import Svg exposing (Svg)
import Svg.Attributes as SvgA


--internal modules

import Tool exposing (..)
import Events
import Pallet
import Messages as Msg exposing (Msg)
import Messages.ClickTarget as ClickTarget exposing (ClickTarget)


symbols =
    [ --Tool Pallet Symbols
      Svg.symbol [ SvgA.id "drawRectangle", SvgA.viewBox "0 0 100 100", SvgA.preserveAspectRatio "none" ]
        --Draw Rectangle button
        [ Svg.use [ SvgA.xlinkHref "#buttonBackground" ] []
        , Svg.use [ SvgA.xlinkHref "#drawRectangleIcon" ] []
        ]
    , Svg.symbol [ SvgA.id "drawElipse", SvgA.viewBox "0 0 100 100", SvgA.preserveAspectRatio "none" ]
        --Draw Elipse button
        [ Svg.use [ SvgA.xlinkHref "#buttonBackground" ] []
        , Svg.use [ SvgA.xlinkHref "#drawElipseIcon" ] []
        ]
    , Svg.symbol [ SvgA.id "drawPolygon", SvgA.viewBox "0 0 100 100", SvgA.preserveAspectRatio "none" ]
        --Draw Polygon button
        [ Svg.use [ SvgA.xlinkHref "#buttonBackground" ] []
        , Svg.use [ SvgA.xlinkHref "#drawPolygonIcon" ] []
        ]

    --Componant symbols--
    , Svg.symbol [ SvgA.id "buttonBackground", SvgA.viewBox "0 0 100 100", SvgA.preserveAspectRatio "none" ]
        [ Svg.rect
            [ SvgA.fill "#ddeaff"
            , SvgA.stroke "none"
            , SvgA.x "0"
            , SvgA.y "0"
            , SvgA.width "100"
            , SvgA.height "100"
            ]
            []
        ]
    , Svg.symbol [ SvgA.id "drawRectangleIcon", SvgA.viewBox "0 0 100 100", SvgA.preserveAspectRatio "xMidYMid" ]
        [ Svg.rect
            [ SvgA.fill "#1e1e1e"
            , SvgA.stroke "none"
            , SvgA.x "20"
            , SvgA.y "20"
            , SvgA.width "60"
            , SvgA.height "60"
            ]
            []
        ]
    , Svg.symbol [ SvgA.id "drawElipseIcon", SvgA.viewBox "0 0 100 100", SvgA.preserveAspectRatio "xMidYMid" ]
        [ Svg.ellipse
            [ SvgA.fill "#1e1e1e"
            , SvgA.stroke "none"
            , SvgA.cx "50"
            , SvgA.cy "50"
            , SvgA.rx "30"
            , SvgA.ry "30"
            ]
            []
        ]
    , Svg.symbol [ SvgA.id "drawPolygonIcon", SvgA.viewBox "0 0 100 100", SvgA.preserveAspectRatio "xMidYMid" ]
        [ Svg.polygon
            [ SvgA.fill "#1e1e1e"
            , SvgA.stroke "none"
            , SvgA.points "50 20, 80 45, 68 80, 32 80, 20 45"
            ]
            []
        ]
    ]



--


toolPallet : Int -> Int -> Float -> Tool -> Svg Msg
toolPallet x y height activeTool =
    let
        buttonList =
            --Determines which buttons appear and in what order
            [ DrawRectangle, DrawElipse, DrawPolygon ]

        borderSize =
            5

        selectionBoxThickness =
            7

        selectionColor =
            "#ff5e5e"

        --Derived values--
        numberOfButtons =
            List.length buttonList

        heightPerButton =
            (height - borderSize) / toFloat numberOfButtons - borderSize

        width =
            heightPerButton + borderSize * 2

        yPosition elementNumber =
            elementNumber * (heightPerButton + borderSize) + toFloat y + borderSize

        buttonWidth =
            heightPerButton

        positionedButtons =
            --Takes the buttonList and converts into as list of Svg Msg
            positionButtons buttonList 0

        positionButtons buttonList index =
            --calls the button helper functions with the index of the button in the buttonList
            case buttonList of
                [] ->
                    []

                tool :: tools ->
                    let
                        f =
                            getToolFunction tool
                    in
                        if tool == activeTool then
                            f index
                                :: selectionBox index
                                :: (positionButtons tools <| index + 1)
                        else
                            f index :: (positionButtons tools <| index + 1)

        getToolFunction tool =
            --maps the buttons in the buttonList to their corrosponding helper functions
            case tool of
                Select ->
                    Debug.crash "TODO implement select button"

                DrawRectangle ->
                    rectangleButton

                DrawElipse ->
                    elipseButton

                DrawPolygon ->
                    polygonButton

        rectangleButton index =
            Svg.use
                [ SvgA.xlinkHref "#drawRectangle" --use the drawRectangle symbol
                , SvgA.x <| toString (x + borderSize)
                , SvgA.y <| toString (yPosition index)
                , SvgA.width <| toString buttonWidth
                , SvgA.height <| toString heightPerButton
                , Events.mouseUpWithClickTarget <| ClickTarget.ToolPallet DrawRectangle
                , Events.mouseDownWithClickTarget <| ClickTarget.ToolPallet DrawRectangle
                ]
                []

        elipseButton index =
            Svg.use
                [ SvgA.xlinkHref "#drawElipse" --use the drawElipse symbol
                , SvgA.x <| toString (x + borderSize)
                , SvgA.y <| toString (yPosition index)
                , SvgA.width <| toString buttonWidth
                , SvgA.height <| toString heightPerButton
                , Events.mouseUpWithClickTarget <| ClickTarget.ToolPallet DrawElipse
                , Events.mouseDownWithClickTarget <| ClickTarget.ToolPallet DrawElipse
                ]
                []

        polygonButton index =
            Svg.use
                [ SvgA.xlinkHref "#drawPolygon" --use the drawPolygon symbol
                , SvgA.x <| toString (x + borderSize)
                , SvgA.y <| toString (yPosition index)
                , SvgA.width <| toString buttonWidth
                , SvgA.height <| toString heightPerButton
                , Events.mouseUpWithClickTarget <| ClickTarget.ToolPallet DrawPolygon
                , Events.mouseDownWithClickTarget <| ClickTarget.ToolPallet DrawPolygon
                ]
                []

        selectionBox index =
            --This function takes the index of the selected tool and draws a selection icon above it
            Svg.g []
                [ Svg.rect
                    --Top Line
                    [ SvgA.x <| toString (x + borderSize)
                    , SvgA.y <| toString (yPosition index)
                    , SvgA.width <| toString buttonWidth
                    , SvgA.height <| toString selectionBoxThickness
                    , SvgA.fill selectionColor
                    , SvgA.stroke "none"
                    ]
                    []
                , Svg.rect
                    --Bottom Line
                    [ SvgA.x <| toString (x + borderSize)
                    , SvgA.y <| toString (yPosition index + heightPerButton - selectionBoxThickness)
                    , SvgA.width <| toString buttonWidth
                    , SvgA.height <| toString selectionBoxThickness
                    , SvgA.fill selectionColor
                    , SvgA.stroke "none"
                    ]
                    []
                , Svg.rect
                    --Left Line
                    [ SvgA.x <| toString (x + borderSize)
                    , SvgA.y <| toString (yPosition index)
                    , SvgA.width <| toString selectionBoxThickness
                    , SvgA.height <| toString heightPerButton
                    , SvgA.fill selectionColor
                    , SvgA.stroke "none"
                    ]
                    []
                , Svg.rect
                    --Right Line
                    [ SvgA.x <| toString (toFloat (x + borderSize) + buttonWidth - selectionBoxThickness)
                    , SvgA.y <| toString (yPosition index)
                    , SvgA.width <| toString selectionBoxThickness
                    , SvgA.height <| toString heightPerButton
                    , SvgA.fill selectionColor
                    , SvgA.stroke "none"
                    ]
                    []
                ]
    in
        Svg.g [] <|
            [ Svg.rect
                --background
                [ SvgA.x <| toString x
                , SvgA.y <| toString y
                , SvgA.width <| toString width
                , SvgA.height <| toString height
                , SvgA.fill "#0254d8"
                , Events.mouseUpWithClickTarget <| ClickTarget.PalletHandle Pallet.ToolPallet
                , Events.mouseDownWithClickTarget <| ClickTarget.PalletHandle Pallet.ToolPallet
                ]
                []
            ]
                ++ positionedButtons
