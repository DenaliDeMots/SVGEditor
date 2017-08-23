module Main exposing (..)

--external modules

import Html
import Html.Events
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Svg.Events as SvgE
import Window
import Mouse
import Platform
import Json.Decode


--internal modules

import Graphic exposing (Graphic)
import Tool exposing (Tool)
import Utilities


--import DrawingTools exposing (Tool)


main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-----------------------------------------------------------------
--Model--


type alias Model =
    --System properties
    { windowSize : Window.Size
    , cursorPosition : CursorPosition
    , mouseDown : Bool
    , currentAction : Action
    , activeTool : Tool
    , graphics : List Graphic.Graphic -- The svg elements on the canvas
    , previewGraphic : Maybe Graphic

    --    , canvas : Graphic
    }



--Model--
---------------------------------------------------------------
--------------------------------------------------------------
--Initialization--


init : Window.Size -> ( Model, Cmd Msg )
init size =
    setWindowSize initialModel size ! []


initialModel =
    { windowSize = { width = 0, height = 0 }
    , cursorPosition = NotTracking
    , mouseDown = False
    , currentAction = None
    , activeTool = Tool.DrawRectangle
    , graphics = []
    , previewGraphic = Maybe.Nothing
    }


setWindowSize model size =
    { model
        | windowSize = size
    }



--Initialization--
----------------------------------------------------------------
---------------------------------------------------------------
--Messeges--


type Msg
    = System SystemMsg


type SystemMsg
    = Resize Window.Size
    | MouseDown ClickTarget Mouse.Position
    | MouseUp ClickTarget Mouse.Position
    | MouseMove Mouse.Position



--Messeges--
----------------------------------------------------------------
----------------------------------------------------------------
--Update--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        System sysMsg ->
            case sysMsg of
                Resize size ->
                    { model | windowSize = size } ! []

                MouseDown clickTarget position ->
                    let
                        ( modelUpdates, commands ) =
                            mouseDownEvent model clickTarget position
                    in
                        { model
                            | mouseDown = True
                            , cursorPosition = modelUpdates.cursorPosition
                            , currentAction = modelUpdates.currentAction
                            , activeTool = modelUpdates.activeTool
                        }
                            ! commands

                MouseUp clickTarget position ->
                    let
                        ( modelUpdates, commands ) =
                            mouseUpEvent model clickTarget position
                    in
                        { model
                            | mouseDown = False
                            , cursorPosition = modelUpdates.cursorPosition
                            , currentAction = modelUpdates.currentAction
                            , graphics = modelUpdates.graphics
                        }
                            ! commands

                MouseMove position ->
                    let
                        ( modelUpdates, commands ) =
                            mouseMoveEvent model position
                    in
                        { model
                            | cursorPosition = modelUpdates.cursorPosition
                            , previewGraphic = modelUpdates.previewGraphic

                            -- , currentAction = modelUpdates.currentAction
                            -- , graphics = modelUpdates.graphics
                        }
                            ! commands


mouseDownEvent model clickTarget position =
    let
        model2 =
            { model | mouseDown = True }
    in
        case Debug.log "mouseDown click target" clickTarget of
            ToolPallet tool ->
                ( { model2
                    | currentAction = None
                    , activeTool = tool
                    , cursorPosition = NotTracking
                  }
                , []
                )

            _ ->
                case model.activeTool of
                    Tool.Select ->
                        case clickTarget of
                            Screen ->
                                ( { model2
                                    | cursorPosition = NotTracking
                                    , currentAction = None
                                  }
                                , []
                                )

                            Graphic graphic ->
                                Debug.crash "TODO - Selected a Graphic for editing"

                            _ ->
                                Debug.log "Evaluated unreachable branch" ( model2, [] )

                    Tool.DrawRectangle ->
                        ( { model2
                            | cursorPosition = Pos position
                            , currentAction = Draw (DrawRect position)
                          }
                        , []
                        )

                    Tool.DrawElipse ->
                        ( { model2
                            | cursorPosition = Pos position
                            , currentAction = Draw (DrawElipse position)
                          }
                        , []
                        )

                    Tool.ToolPalletHandle ->
                        Debug.crash "TODO - Moving Tool Pallet"


mouseUpEvent model clickTarget position =
    case model.currentAction of
        None ->
            ( { model
                | cursorPosition = NotTracking
              }
            , []
            )

        Draw drawAction ->
            case drawAction of
                DrawRect startPosition ->
                    ( { model
                        | currentAction = None
                        , cursorPosition = NotTracking
                        , graphics = model.graphics ++ Utilities.maybeToList model.previewGraphic
                        , previewGraphic = Maybe.Nothing
                      }
                    , []
                    )

                DrawElipse startPosition ->
                    ( { model
                        | currentAction = None
                        , cursorPosition = NotTracking
                        , graphics = model.graphics ++ Utilities.maybeToList model.previewGraphic
                        , previewGraphic = Maybe.Nothing
                      }
                    , []
                    )


mouseMoveEvent model position =
    let
        model2 =
            { model
                | cursorPosition =
                    if model.cursorPosition == NotTracking then
                        NotTracking
                    else
                        Pos position
            }
    in
        case model2.currentAction of
            None ->
                ( model2, [] )

            Draw drawAction ->
                ( updatePreviewGraphic drawAction position model2, [] )


updatePreviewGraphic drawAction currentPosition model =
    case drawAction of
        DrawRect startPosition ->
            { model
                | previewGraphic = List.head (createRectangle startPosition currentPosition model)
            }

        DrawElipse startPosition ->
            { model
                | previewGraphic = List.head (createElipse startPosition currentPosition model)
            }



--Create Graphic Functions--


createRectangle start end model =
    let
        recAttributes =
            { x = toFloat <| min start.x end.x -- Upper left x coordinate
            , y = toFloat <| min start.y end.y -- Upper left y coordinate
            , width = toFloat <| abs (start.x - end.x)
            , height = toFloat <| abs (start.y - end.y)
            , rx = 0 -- Corner x rounding radius
            , ry = 0 -- Corner y rounding radius
            }

        commonAttributes =
            { stroke = "none"
            , fill = "#666"
            , strokeWidth = "0"
            }
    in
        if recAttributes.height == 0 || recAttributes.width == 0 then
            []
        else
            [ Graphic.createRectangle recAttributes commonAttributes ]


createElipse startPosition currentPosition model =
    let
        xRadius =
            abs (startPosition.x - currentPosition.x) |> toFloat |> flip (/) 2

        yRadius =
            abs (startPosition.y - currentPosition.y) |> toFloat |> flip (/) 2

        elipseAttributes =
            { rx = xRadius
            , ry = yRadius
            , cx = toFloat (max startPosition.x currentPosition.x) - (xRadius) -- x center coordinate
            , cy = toFloat (max startPosition.y currentPosition.y) - (yRadius) -- y center coordinate
            }

        commonAttributes =
            { stroke = "none"
            , fill = "#666"
            , strokeWidth = "0"
            }
    in
        if elipseAttributes.rx == 0 || elipseAttributes.ry == 0 then
            []
        else
            [ Graphic.createElipse elipseAttributes commonAttributes ]



-------------------------------------------------------------------
-------------------------------------------------------------------
--Subscriptions--


subscriptions : Model -> Sub Msg
subscriptions model =
    [ Window.resizes (System << Resize)
    , Mouse.downs (System << MouseDown Screen)
    , Mouse.ups (System << MouseUp Screen)
    , trackPosition model.cursorPosition (System << MouseMove)
    ]
        |> Sub.batch


trackPosition cursor tagger =
    case cursor of
        NotTracking ->
            Sub.none

        _ ->
            Mouse.moves tagger



--Subscriptions--
--------------------------------------------------------------------
--------------------------------------------------------------------
--View--


view : Model -> Svg Msg
view model =
    let
        wHeight =
            model.windowSize.height

        wWidth =
            model.windowSize.width
    in
        Svg.svg
            [ SvgA.width <| toString wWidth
            , SvgA.height <| toString wHeight
            , SvgA.viewBox <| "0 0 " ++ toString wWidth ++ " " ++ toString wHeight
            , onMouseUpIsolated (System << MouseUp Screen)
            , onMouseDownIsolated (System << MouseDown Screen)
            ]
        <|
            symbols
                ++ (model.graphics
                        ++ Utilities.maybeToList model.previewGraphic
                        |> List.map
                            (\graphic ->
                                Graphic.toSvg
                                    [ onMouseUpIsolated <| System << (MouseUp <| Graphic graphic)
                                    , onMouseDownIsolated <| System << (MouseDown <| Graphic graphic)
                                    ]
                                    graphic
                            )
                        |> flip (++) [ toolPallet 50 50 80 160 model.activeTool ]
                   )



--Interface Elements--


symbols =
    [ --Tool Pallet Symbols
      Svg.symbol [ SvgA.id "drawRectangle", SvgA.viewBox "0 0 100 100" ]
        [ Svg.rect
            [ SvgA.fill "#ddeaff"
            , SvgA.x "1"
            , SvgA.y "1"
            , SvgA.width "98"
            , SvgA.height "98"
            ]
            []
        , Svg.rect [ SvgA.fill "#1e1e1e", SvgA.x "20", SvgA.y "20", SvgA.width "60", SvgA.height "60" ] []
        ]
    , Svg.symbol [ SvgA.id "drawElipse", SvgA.viewBox "0 0 100 100" ]
        [ Svg.rect
            [ SvgA.fill "#ddeaff"
            , SvgA.x "1"
            , SvgA.y "1"
            , SvgA.width "98"
            , SvgA.height "98"
            ]
            []
        , Svg.ellipse [ SvgA.fill "#1e1e1e", SvgA.cx "50", SvgA.cy "50", SvgA.rx "30", SvgA.ry "30" ] []
        ]
    ]


toolPallet : Int -> Int -> Int -> Int -> Tool -> Svg Msg
toolPallet x y width height activeTool =
    let
        toolType tool =
            if tool == activeTool then
                "#ff5e5e"
            else
                "none"

        borderSize =
            5

        numberOfButtons =
            2

        heightPerButton =
            toFloat (height - borderSize) / numberOfButtons

        yPosition elementNumber =
            elementNumber * heightPerButton + borderSize + toFloat y

        xWidth =
            width - borderSize * 2
    in
        Svg.g []
            [ Svg.rect
                --background
                [ SvgA.x <| toString x
                , SvgA.y <| toString y
                , SvgA.width <| toString width
                , SvgA.height <| toString height
                , SvgA.fill "#0254d8"
                , onMouseUpIsolated (System << MouseUp (ToolPallet Tool.ToolPalletHandle))
                , onMouseDownIsolated (System << MouseDown (ToolPallet Tool.ToolPalletHandle))
                ]
                []
            , Svg.use
                [ SvgA.xlinkHref "#drawRectangle"
                , SvgA.x <| toString (x + borderSize)
                , SvgA.y <| toString (yPosition 0)
                , SvgA.width <| toString xWidth
                , SvgA.height <| toString heightPerButton
                , SvgA.stroke <| toolType Tool.DrawRectangle
                , onMouseUpIsolated (System << MouseUp (ToolPallet Tool.DrawRectangle))
                , onMouseDownIsolated (System << MouseDown (ToolPallet Tool.DrawRectangle))
                ]
                []
            , Svg.use
                [ SvgA.xlinkHref "#drawElipse"
                , SvgA.x <| toString (x + borderSize)
                , SvgA.y <| toString (yPosition 1)
                , SvgA.width <| toString xWidth
                , SvgA.height <| toString heightPerButton
                , SvgA.stroke <| toolType Tool.DrawElipse
                , onMouseUpIsolated (System << MouseUp (ToolPallet Tool.DrawElipse))
                , onMouseDownIsolated (System << MouseDown (ToolPallet Tool.DrawElipse))
                ]
                []
            ]



--onMouseUpIsolated (System << MouseUp Screen)
--onMouseDownIsolated (System << MouseDown Screen)
--onMouseUpIsolated <| System << (MouseUp <| Graphic graphic)
--onMouseDownIsolated <| System << (MouseDown <| Graphic graphic)
--View--
----------------------------------------------------------------------
-----------------------------------------------------------------------
--Other Code--
--Data Types--


type CursorPosition
    = NotTracking
    | Pos Position


type alias Position =
    { x : Int, y : Int }


type ClickTarget
    = Graphic Graphic.Graphic
    | Screen
    | ToolPallet Tool


type Action
    = None
    | Draw DrawAction


type DrawAction
    = DrawRect Position
    | DrawElipse Position



--Custom Events--


onMouseDownIsolated tagger =
    Html.Events.onWithOptions
        "mousedown"
        { stopPropagation = True, preventDefault = True }
        (Json.Decode.map tagger Mouse.position)


onMouseUpIsolated tagger =
    Html.Events.onWithOptions
        "mouseup"
        { stopPropagation = True, preventDefault = True }
        (Json.Decode.map tagger Mouse.position)
