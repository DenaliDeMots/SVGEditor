module Graphic exposing (Graphic, toSvg, createRectangle, createElipse, CommonAttributes)

import Svg
import Svg.Attributes as SvgA
import Svg.Events as SvgE


type Graphic
    = Rectangle RectangleAttributes CommonAttributes
    | Elipse ElipseAttributes CommonAttributes
    | Polygon (List ( Float, Float )) CommonAttributes


type alias RectangleAttributes =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , rx : Float
    , ry : Float
    }


type alias ElipseAttributes =
    { cx : Float
    , cy : Float
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


toSvg : List (Svg.Attribute msg) -> Graphic -> Svg.Svg msg
toSvg extraAttributes graphic =
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
                    ++ extraAttributes
                )
                []

        Elipse elipse common ->
            Svg.ellipse
                ([ SvgA.cx <| toString elipse.cx
                 , SvgA.cy <| toString elipse.cy
                 , SvgA.rx <| toString elipse.rx
                 , SvgA.ry <| toString elipse.ry
                 ]
                    ++ commonToSvgA common
                    ++ extraAttributes
                )
                []

        Polygon points common ->
            Svg.polygon ([ SvgA.points <| pointListToString points ] ++ commonToSvgA common ++ extraAttributes) []


pointListToString pList =
    case pList of
        [] ->
            ""

        --unreachable unless the list is empty
        ( x, y ) :: [] ->
            toString x ++ " " ++ toString y

        ( x, y ) :: ps ->
            toString x ++ " " ++ toString y ++ ", " ++ pointListToString ps


commonToSvgA : CommonAttributes -> List (Svg.Attribute msg)
commonToSvgA common =
    [ SvgA.fill common.fill
    , SvgA.stroke common.stroke
    , SvgA.strokeWidth common.strokeWidth

    --   , SvgA.transform <| toString common.transform
    ]


createRectangle : RectangleAttributes -> CommonAttributes -> Graphic
createRectangle rec common =
    Rectangle rec common


createElipse : ElipseAttributes -> CommonAttributes -> Graphic
createElipse rec common =
    Elipse rec common
