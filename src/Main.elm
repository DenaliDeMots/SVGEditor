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
import Color
import Color.Convert as CC
import Result


--internal modules

import Graphic exposing (Graphic)
import Tool exposing (Tool)
import Tool.Render
import Properties exposing (PropertyWidget, PropertyPalletState)
import Properties.Render
import Utilities
import Events
import Messages as Msg exposing (Msg)
import Messages.ClickTarget as ClickTarget exposing (ClickTarget)
import Messages.UpdatePropertyPallet as PPS


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
    , propertyPalletState : PropertyPalletState
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
    , propertyPalletState = initialPropertyPalletState
    , graphics = []
    , previewGraphic = Maybe.Nothing
    }


setWindowSize model size =
    { model
        | windowSize = size
    }


initialPropertyPalletState =
    { fillColor = Color.green
    , strokeColor = Color.blue
    , strokeWidth = 4
    }



--Initialization--
----------------------------------------------------------------
---------------------------------------------------------------
--Messeges--
--Messeges--
----------------------------------------------------------------
----------------------------------------------------------------
--Update--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg.Resize size ->
            { model | windowSize = size } ! []

        Msg.UpdatePropertyPalletState stateMsg ->
            let
                palletState =
                    model.propertyPalletState

                nextModel state =
                    { model | propertyPalletState = state }
            in
                case stateMsg of
                    PPS.FillColor color ->
                        nextModel { palletState | fillColor = color } ! [ Cmd.none ]

                    PPS.StrokeColor color ->
                        nextModel { palletState | strokeColor = color } ! [ Cmd.none ]

                    PPS.StrokeWidth result ->
                        nextModel { palletState | strokeWidth = Result.withDefault palletState.strokeWidth result } ! [ Cmd.none ]

        Msg.MouseDown clickTarget position ->
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

        Msg.MouseUp clickTarget position ->
            let
                ( modelUpdates, commands ) =
                    mouseUpEvent model clickTarget position
            in
                { model
                    | mouseDown = False
                    , cursorPosition = modelUpdates.cursorPosition
                    , currentAction = modelUpdates.currentAction
                    , graphics = modelUpdates.graphics
                    , previewGraphic = modelUpdates.previewGraphic
                }
                    ! commands

        Msg.MouseMove position ->
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
            ClickTarget.ToolPallet tool ->
                ( { model2
                    | currentAction = None
                    , activeTool = tool
                    , cursorPosition = NotTracking
                  }
                , []
                )

            ClickTarget.ToolPalletHandle ->
                Debug.crash "TODO implement tool pallet dragging"

            _ ->
                case model.activeTool of
                    Tool.Select ->
                        case clickTarget of
                            ClickTarget.Screen ->
                                ( { model2
                                    | cursorPosition = NotTracking
                                    , currentAction = None
                                  }
                                , []
                                )

                            ClickTarget.Graphic graphic ->
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

                    Tool.DrawPolygon ->
                        let
                            ( startPoint, posList ) =
                                case model2.currentAction of
                                    Draw (DrawPolygon start list) ->
                                        ( start, list )

                                    _ ->
                                        ( Utilities.intPositionToFloat position, [] )
                        in
                            ( { model2
                                | cursorPosition = Pos position
                                , currentAction = Draw (DrawPolygon startPoint posList)
                                , previewGraphic = List.head <| createPolygon startPoint posList model
                              }
                            , []
                            )


mouseUpEvent : Model -> ClickTarget -> Position -> ( Model, List (Cmd Msg) )
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

                DrawPolygon startPoint posList ->
                    let
                        floatPosition =
                            Utilities.intPositionToFloat position

                        distanceFromStartPoint =
                            Utilities.distance startPoint floatPosition

                        numberOfPoints =
                            List.length posList

                        --ignore creating a point if this is the upstroke of the initial click
                        firstPoint =
                            floatPosition == startPoint && posList == []

                        completedPolygon =
                            numberOfPoints >= 2 && distanceFromStartPoint <= Graphic.polygonSnapDistance
                    in
                        if completedPolygon then
                            ( { model
                                | currentAction = None
                                , cursorPosition = NotTracking
                                , graphics = model.graphics ++ createPolygon startPoint posList model
                                , previewGraphic = Maybe.Nothing
                              }
                            , []
                            )
                        else
                            ( { model
                                | currentAction =
                                    Draw <|
                                        DrawPolygon startPoint
                                            (if firstPoint then
                                                posList
                                             else
                                                floatPosition :: posList
                                            )
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

        DrawPolygon startPoint posList ->
            let
                floatPosition =
                    Utilities.intPositionToFloat currentPosition

                snapToStart =
                    Utilities.distance startPoint floatPosition <= Graphic.polygonSnapDistance && List.length posList >= 2
            in
                { model
                    | previewGraphic =
                        List.head <|
                            createPolygon startPoint
                                (if snapToStart then
                                    posList
                                 else
                                    floatPosition :: posList
                                )
                                model
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
            { stroke = CC.colorToHex model.propertyPalletState.strokeColor
            , fill = CC.colorToHex model.propertyPalletState.fillColor
            , strokeWidth = toString model.propertyPalletState.strokeWidth
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
            { stroke = CC.colorToHex model.propertyPalletState.strokeColor
            , fill = CC.colorToHex model.propertyPalletState.fillColor
            , strokeWidth = toString model.propertyPalletState.strokeWidth
            }
    in
        if elipseAttributes.rx == 0 || elipseAttributes.ry == 0 then
            []
        else
            [ Graphic.createElipse elipseAttributes commonAttributes ]


createPolygon : { x : Float, y : Float } -> List { x : Float, y : Float } -> Model -> List Graphic
createPolygon startPoint polyList model =
    let
        commonAttributes =
            { stroke = CC.colorToHex model.propertyPalletState.strokeColor
            , fill = CC.colorToHex model.propertyPalletState.fillColor
            , strokeWidth = toString model.propertyPalletState.strokeWidth
            }
    in
        [ Graphic.createPolygon startPoint polyList commonAttributes ]



-------------------------------------------------------------------
-------------------------------------------------------------------
--Subscriptions--


subscriptions : Model -> Sub Msg
subscriptions model =
    [ Window.resizes (Msg.Resize)
    , Mouse.downs (Msg.MouseDown ClickTarget.Screen)
    , Mouse.ups (Msg.MouseUp ClickTarget.Screen)
    , trackPosition model.cursorPosition (Msg.MouseMove)
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
            , Events.mouseUpWithClickTarget <| ClickTarget.Screen
            , Events.mouseDownWithClickTarget <| ClickTarget.Screen
            ]
        <|
            symbols
                ++ (model.graphics
                        ++ Utilities.maybeToList model.previewGraphic
                        |> List.map
                            (\graphic ->
                                Graphic.toSvg
                                    [ Events.mouseDownWithClickTarget <| ClickTarget.Graphic graphic
                                    , Events.mouseUpWithClickTarget <| ClickTarget.Graphic graphic
                                    ]
                                    graphic
                            )
                        |> flip (++) [ Tool.Render.toolPallet 50 50 190 model.activeTool ]
                        |> flip (++) [ Properties.Render.propertiesPallet 400 50 170 model.propertyPalletState ]
                   )



--SVG Symbol Elements


symbols =
    Tool.Render.symbols



----------------------------------------------------------------------
-----------------------------------------------------------------------
--Other Code--
--Data Types--


type CursorPosition
    = NotTracking
    | Pos Position


type alias Position =
    { x : Int, y : Int }


type Action
    = None
    | Draw DrawAction


type DrawAction
    = DrawRect Position
    | DrawElipse Position
    | DrawPolygon { x : Float, y : Float } (List { x : Float, y : Float })
