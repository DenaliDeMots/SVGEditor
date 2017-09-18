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
import Action exposing (..)
import Tool exposing (Tool)
import Tool.Render
import Properties exposing (PropertyWidget, PropertyPalletState)
import Properties.Render
import Utilities
import Events
import Messages as Msg exposing (Msg)
import Messages.ClickTarget as ClickTarget exposing (ClickTarget)
import Messages.UpdatePropertyPallet as PPS
import Pallet exposing (Pallet)


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
    , toolPalletPosition : PalletPosition
    , propertyPalletPosition : PalletPosition
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
    (initialModel
        |> setWindowSize size
        |> setPalletLocations size
    )
        ! []


initialModel =
    { windowSize = { width = 0, height = 0 }
    , cursorPosition = NotTracking
    , mouseDown = False
    , currentAction = None
    , activeTool = Tool.DrawRectangle
    , toolPalletPosition = { x = 0, y = 0, height = 0 }
    , propertyPalletPosition = { x = 0, y = 0, height = 0 }
    , propertyPalletState = initialPropertyPalletState
    , graphics = []
    , previewGraphic = Maybe.Nothing
    }


setWindowSize size model =
    { model
        | windowSize = size
    }


setPalletLocations size model =
    { model
        | toolPalletPosition =
            { x = toFloat size.width * 0.05 |> round
            , y = toFloat size.height * 0.05 |> round
            , height = 190
            }
        , propertyPalletPosition =
            { x = toFloat size.width * 0.95 - 85 |> round
            , y = toFloat size.height * 0.05 |> round
            , height = 170
            }
    }


initialPropertyPalletState =
    { fillColor = Color.green
    , strokeColor = Color.blue
    , strokeWidth = 4
    }



--Initialization--
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
                    PPS.UpdateFillColor color ->
                        let
                            nextPalletState =
                                { palletState | fillColor = color }
                        in
                            nextModel nextPalletState ! []

                    PPS.UpdateStrokeColor color ->
                        let
                            nextPalletState =
                                { palletState | fillColor = color }
                        in
                            nextModel nextPalletState ! []

                    PPS.StrokeWidth result ->
                        nextModel { palletState | strokeWidth = Result.withDefault palletState.strokeWidth result } ! []

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
                    , propertyPalletState = modelUpdates.propertyPalletState
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
                    , propertyPalletPosition = modelUpdates.propertyPalletPosition
                    , toolPalletPosition = modelUpdates.toolPalletPosition
                }
                    ! commands

        Msg.MouseOut clickTarget ->
            let
                ( modelUpdates, commands ) =
                    mouseOutEvent model clickTarget
            in
                { model
                    | cursorPosition = modelUpdates.cursorPosition
                }
                    ! commands


mouseDownEvent : Model -> ClickTarget -> Position -> ( Model, List (Cmd Msg) )
mouseDownEvent model clickTarget position =
    let
        model2 =
            { model | mouseDown = True }
    in
        case Debug.log "Click Target " clickTarget of
            ClickTarget.ToolPallet tool ->
                ( { model2
                    | currentAction = None
                    , activeTool = tool
                    , cursorPosition = NotTracking
                  }
                , []
                )

            ClickTarget.PalletHandle pallet ->
                let
                    offset =
                        Utilities.getOffset position
                            (case pallet of
                                Pallet.ToolPallet ->
                                    { x = model.toolPalletPosition.x, y = model.toolPalletPosition.y }

                                Pallet.PropertiesPallet ->
                                    { x = model.propertyPalletPosition.x, y = model.propertyPalletPosition.y }
                            )
                in
                    ( { model2
                        | currentAction = MovePallet pallet offset
                        , cursorPosition = Pos position
                      }
                    , []
                    )

            ClickTarget.PropertiesPallet widget ->
                let
                    propPalletState =
                        model.propertyPalletState
                in
                    case widget of
                        Properties.FillColor pickerWidget ->
                            let
                                newFillColor color =
                                    { propPalletState | fillColor = color }
                            in
                                case pickerWidget of
                                    Properties.SaturationLightness posToColor ->
                                        ( { model2
                                            | currentAction = SelectFillColor DraggingSatLight
                                            , propertyPalletState = newFillColor (posToColor position)
                                            , cursorPosition = Pos position
                                          }
                                        , []
                                        )

                                    Properties.Hue posToColor ->
                                        ( { model2
                                            | currentAction = SelectFillColor DraggingHue
                                            , propertyPalletState = newFillColor (posToColor position)
                                            , cursorPosition = Pos position
                                          }
                                        , []
                                        )

                                    Properties.Background ->
                                        ( model2, [] )

                                    Properties.ColorBox ->
                                        ( { model2
                                            | currentAction =
                                                case model2.currentAction of
                                                    SelectFillColor _ ->
                                                        None

                                                    _ ->
                                                        SelectFillColor NotDragging
                                          }
                                        , []
                                        )

                        Properties.StrokeColor pickerWidget ->
                            let
                                newStrokeColor color =
                                    { propPalletState | strokeColor = color }
                            in
                                case pickerWidget of
                                    Properties.SaturationLightness posToColor ->
                                        ( { model2
                                            | currentAction = SelectStrokeColor DraggingSatLight
                                            , propertyPalletState = newStrokeColor (posToColor position)
                                          }
                                        , []
                                        )

                                    Properties.Hue posToColor ->
                                        ( { model2
                                            | currentAction = SelectStrokeColor DraggingHue
                                            , propertyPalletState = newStrokeColor (posToColor position)
                                          }
                                        , []
                                        )

                                    Properties.Background ->
                                        ( model2, [] )

                                    Properties.ColorBox ->
                                        ( { model2
                                            | currentAction =
                                                case model2.currentAction of
                                                    SelectStrokeColor _ ->
                                                        None

                                                    _ ->
                                                        SelectStrokeColor NotDragging
                                          }
                                        , []
                                        )

                        Properties.StrokeWidth button ->
                            case button of
                                Properties.Increment ->
                                    let
                                        pPS =
                                            model2.propertyPalletState

                                        newPPS =
                                            { pPS | strokeWidth = pPS.strokeWidth + 1 }
                                    in
                                        ( { model2 | propertyPalletState = newPPS }, [] )

                                Properties.Decrement ->
                                    let
                                        pPS =
                                            model2.propertyPalletState

                                        newPPS =
                                            { pPS | strokeWidth = pPS.strokeWidth - 1 |> clamp 0 200 }
                                    in
                                        ( { model2 | propertyPalletState = newPPS }, [] )

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
                                , previewGraphic = List.head <| createPolygon startPoint posList model2
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

        MovePallet pallet offset ->
            ( { model
                | currentAction = None
                , cursorPosition = NotTracking
              }
            , []
            )

        SelectFillColor pickerAction ->
            ( { model
                | cursorPosition = NotTracking
                , currentAction = SelectFillColor NotDragging
              }
            , []
            )

        SelectStrokeColor pickerAction ->
            ( { model
                | cursorPosition = NotTracking
                , currentAction = SelectStrokeColor NotDragging
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

            MovePallet pallet offset ->
                let
                    newPosition =
                        Utilities.applyOffset offset position
                in
                    ( { model2
                        | currentAction = MovePallet Pallet.ToolPallet offset
                        , cursorPosition = Pos position
                        , toolPalletPosition =
                            if pallet == Pallet.ToolPallet then
                                { x = newPosition.x, y = newPosition.y, height = model2.toolPalletPosition.height }
                            else
                                model2.toolPalletPosition
                        , propertyPalletPosition =
                            if pallet == Pallet.PropertiesPallet then
                                { x = newPosition.x, y = newPosition.y, height = model2.propertyPalletPosition.height }
                            else
                                model2.propertyPalletPosition
                      }
                    , []
                    )

            SelectFillColor pickerAction ->
                ( model2, [] )

            SelectStrokeColor pickerAction ->
                ( model2, [] )

            Draw drawAction ->
                ( updatePreviewGraphic drawAction position model2, [] )


mouseOutEvent : Model -> ClickTarget -> ( Model, List (Cmd Msg) )
mouseOutEvent model clickTarget =
    case clickTarget of
        PropertiesPallet widget ->
            let
                stopTracking rec =
                    { rec | cursorPosition = NotTracking }

                mouseOutPickerWidget widget model =
                    case widget of
                        SaturationLightness _ ->
                            stopTracking model

                        Hue _ ->
                            stopTracking model

                        _ ->
                            model
            in
                case widget of
                    FillColor pickerWidget ->
                        ( mouseOutPickerWidget pickerWidget model, [] )

                    StrokeColor pickerWidget ->
                        ( mouseOutPickerWidget pickerWidget model, [] )

                    _ ->
                        ( model, [] )

        _ ->
            ( model, [] )


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

        toolPalletPosition =
            model.toolPalletPosition

        propertyPalletPosition =
            model.propertyPalletPosition

        activeTool =
            model.activeTool

        propertyPalletState =
            model.propertyPalletState
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
                        |> flip (++)
                            [ Tool.Render.toolPallet
                                toolPalletPosition.x
                                toolPalletPosition.y
                                toolPalletPosition.height
                                activeTool
                            ]
                        |> flip (++)
                            [ Properties.Render.propertiesPallet
                                propertyPalletPosition.x
                                propertyPalletPosition.y
                                propertyPalletPosition.height
                                propertyPalletState
                                model.currentAction
                            ]
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


type alias PalletPosition =
    { x : Int, y : Int, height : Float }
