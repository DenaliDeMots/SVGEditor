module Graphic exposing (Graphic, toSvg, createRectangle)

import Svg
import Svg.Attributes as SvgA
import Svg.Events as SvgE


type Graphic
    = Rectangle RectangleAttributes CommonAttributes


type alias RectangleAttributes =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , rx : Float
    , ry : Float
    }


type alias CommonAttributes =
    { stroke : String
    , strokeWidth : String
    , fill : String

    --, transform : Transform
    }


type Transform
    = None
    | Translate Float Float {- X, Y -} Transform
    | Scale Float Float {- X, Y -} Transform
    | Rotate Float Float Float {- degrees, X, Y -} Transform
    | SkewX Float {- degrees -} Transform
    | SkewY Float {- degrees -} Transform



--helper functions


toSvg : Graphic -> Svg.Svg msg
toSvg graphic =
    case graphic of
        Rectangle rec common ->
            Svg.rect
                ([ SvgA.x <| toString rec.x
                 , SvgA.y <| toString rec.y
                 , SvgA.width <| toString rec.width
                 , SvgA.height <| toString rec.height
                 , SvgA.rx <| toString rec.rx
                 , SvgA.ry <| toString rec.ry
                 ]
                    ++ commonToSvgA common
                )
                []


commonToSvgA : CommonAttributes -> List (Svg.Attribute msg)
commonToSvgA common =
    [ SvgA.fill <| toString common.fill
    , SvgA.stroke <| toString common.stroke
    , SvgA.strokeWidth <| toString common.strokeWidth

    --   , SvgA.transform <| toString common.transform
    ]


createRectangle : RectangleAttributes -> CommonAttributes -> Graphic
createRectangle rec common =
    Rectangle rec common
