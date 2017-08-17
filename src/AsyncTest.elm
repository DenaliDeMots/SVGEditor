module AsyncTest exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
import AnimationFrame
import Time
import Task
import Process
import Utilities


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { angle : Float
    , centerPointPosition : ( Int, Int )
    , radius : Float
    , rotationRate : Time.Time
    , result : Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model 0 ( 300, 300 ) 60 1 0, Cmd.none )



-- UPDATE


type Msg
    = Diff Float
    | Calculate
    | CalculateAsync
    | Result Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Diff diff ->
            { model | angle = nextAngle model diff } => Cmd.none

        Calculate ->
            { model
                | result =
                    Utilities.until predicate updater initialState
            }
                ! []

        CalculateAsync ->
            model ! [ Task.perform Result <| Process.spawn (Utilities.until_A predicate updater initialState) ]

        Result x ->
            { model | result = x } ! []



--Calculation stuff


predicate x =
    x > 100000


updater x =
    x + 1


initialState =
    0



--Animation Stuff


nextAngle model diff =
    let
        currentAngle =
            .angle model

        rotationRate =
            .rotationRate model
    in
        (rotationRate * 2 * pi * Time.inSeconds diff + currentAngle)
            |> normalizeAngle


normalizeAngle angle =
    if angle < turns 1 then
        angle
    else
        normalizeAngle (angle - turns 1)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Diff



-- VIEW


(=>) =
    (,)


view : Model -> Html Msg
view model =
    let
        ( x, y ) =
            absolutePosition model
    in
        div []
            [ div
                [ style
                    [ "background-color" => "#3C8D2F"
                    , "cursor" => "move"
                    , "width" => "100px"
                    , "height" => "100px"
                    , "border-radius" => "4px"
                    , "position" => "absolute"
                    , "left" => px x
                    , "top" => px y
                    , "color" => "white"
                    , "display" => "flex"
                    , "align-items" => "center"
                    , "justify-content" => "center"
                    ]
                ]
                []
            , Html.button [ onClick Calculate ] [ Html.text "Big Calculation" ]
            , Html.button [ onClick CalculateAsync ] [ Html.text "Big Calculation Async" ]
            , Html.button [ onClick (Result 0) ] [ Html.text "Clear" ]
            , text <| toString <| .result model
            ]


px : Int -> String
px number =
    toString number ++ "px"


absolutePosition : Model -> ( Int, Int )
absolutePosition { angle, centerPointPosition, radius } =
    let
        ( offsetX, offsetY ) =
            fromPolar ( radius, angle )

        ( x, y ) =
            centerPointPosition
    in
        ( offsetX + toFloat x |> round, offsetY + toFloat y |> round )
