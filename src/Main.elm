module Main exposing (..)

--external modules

import Html
import Html.Events
import Svg
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
        case clickTarget of
            ToolPallet tool ->
                ( { model2
                    | currentAction = None
                    , activeTool = tool
                    , cursorPosition = Pos position
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
                        Debug.crash "TODO"

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



--Create Graphic Functions--


createRectangle start end model =
    let
        topLeftX =
            toFloat <| min start.x end.x

        topLeftY =
            toFloat <| min start.y end.y

        width =
            toFloat <| abs (start.x - end.x)

        height =
            toFloat <| abs (start.y - end.y)

        recAttributes =
            { x = topLeftX
            , y = topLeftY
            , width = width
            , height = height
            , rx = 0
            , ry = 0
            }

        commonAttributes =
            { stroke = "none"
            , fill = "#666"
            , strokeWidth = "0"
            }
    in
        if height == 0 || width == 0 then
            []
        else
            [ Graphic.createRectangle recAttributes commonAttributes ]



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


view : Model -> Svg.Svg Msg
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
            (List.map Graphic.toSvg <| model.graphics ++ Utilities.maybeToList model.previewGraphic)



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
    | ToolPallet Tool.Tool


type Action
    = None
    | Draw DrawAction



--|SelectTool


type DrawAction
    = DrawRect Position



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
