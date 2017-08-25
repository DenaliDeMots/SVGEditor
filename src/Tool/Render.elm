module Tool.Render exposing (..)

--external modules

import Svg exposing (Svg)
import Svg.Attributes as SvgA


--internal modules

import Tool exposing (..)
import Events
import Messages as Msg exposing (Msg)
import Messages.ClickTarget as ClickTarget exposing (ClickTarget)


symbols =
    [ --Tool Pallet Symbols
      Svg.symbol [ SvgA.id "drawRectangle", SvgA.viewBox "0 0 100 100", SvgA.preserveAspectRatio "none" ]
        [ Svg.use [ SvgA.xlinkHref "#buttonBackground" ] []
        , Svg.use [ SvgA.xlinkHref "#drawRectangleIcon" ] []
        ]
    , Svg.symbol [ SvgA.id "drawElipse", SvgA.viewBox "0 0 100 100", SvgA.preserveAspectRatio "none" ]
        [ Svg.use [ SvgA.xlinkHref "#buttonBackground" ] []
        , Svg.use [ SvgA.xlinkHref "#drawElipseIcon" ] []
        ]
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
    ]



--


toolPallet : Int -> Int -> Int -> Int -> Tool -> Svg Msg
toolPallet x y width height activeTool =
    let
        borderSize =
            5

        numberOfButtons =
            2

        heightPerButton =
            toFloat (height - borderSize) / numberOfButtons - borderSize

        yPosition elementNumber =
            elementNumber * (heightPerButton + borderSize) + toFloat y + borderSize

        xWidth =
            width - borderSize * 2

        selectionBoxThickness =
            7

        selectionColor =
            "#ff5e5e"

        buttonList =
            --Determines which buttons appear and in what order
            [ DrawRectangle, DrawElipse ]

        getToolFunction tool =
            case tool of
                Select ->
                    Debug.crash "TODO implement select button"

                DrawRectangle ->
                    rectangleButton

                DrawElipse ->
                    elipseButton

                ToolPalletHandle ->
                    Debug.crash "TODO implement draggable tool pallet"

        positionedButtons =
            positionButtons buttonList 0

        positionButtons buttonList index =
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

        rectangleButton index =
            Svg.use
                [ SvgA.xlinkHref "#drawRectangle"
                , SvgA.x <| toString (x + borderSize)
                , SvgA.y <| toString (yPosition index)
                , SvgA.width <| toString xWidth
                , SvgA.height <| toString heightPerButton
                , Events.mouseUpWithClickTarget <| ClickTarget.ToolPallet DrawRectangle
                , Events.mouseDownWithClickTarget <| ClickTarget.ToolPallet DrawRectangle
                ]
                []

        elipseButton index =
            Svg.use
                [ SvgA.xlinkHref "#drawElipse"
                , SvgA.x <| toString (x + borderSize)
                , SvgA.y <| toString (yPosition index)
                , SvgA.width <| toString xWidth
                , SvgA.height <| toString heightPerButton
                , Events.mouseUpWithClickTarget <| ClickTarget.ToolPallet DrawElipse
                , Events.mouseDownWithClickTarget <| ClickTarget.ToolPallet DrawElipse
                ]
                []

        selectionBox index =
            Svg.g []
                --Selection icon to draw on top of selected tool button
                [ Svg.rect
                    --Top Line
                    [ SvgA.x <| toString (x + borderSize)
                    , SvgA.y <| toString (yPosition index)
                    , SvgA.width <| toString xWidth
                    , SvgA.height <| toString selectionBoxThickness
                    , SvgA.fill selectionColor
                    , SvgA.stroke "none"
                    ]
                    []
                , Svg.rect
                    --Bottom Line
                    [ SvgA.x <| toString (x + borderSize)
                    , SvgA.y <| toString (yPosition index + heightPerButton - selectionBoxThickness)
                    , SvgA.width <| toString xWidth
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
                    [ SvgA.x <| toString (x + borderSize + xWidth - selectionBoxThickness)
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
                , Events.mouseUpWithClickTarget <| ClickTarget.ToolPallet ToolPalletHandle
                , Events.mouseDownWithClickTarget <| ClickTarget.ToolPallet DrawElipse
                ]
                []
            ]
                ++ positionedButtons
