module Graphic exposing (Graphic, toSvg, createRectangle, createElipse, createPolygon, polygonSnapDistance, CommonAttributes)

--This module defines the Graphic data type which is used to represent the drawn graphics in the model

import Svg
import Svg.Attributes as SvgA
import Svg.Events as SvgE


type Graphic
    = Rectangle RectangleAttributes CommonAttributes
    | Elipse ElipseAttributes CommonAttributes
    | Polygon StartPosition PositionList CommonAttributes


type alias PositionList =
    List { x : Float, y : Float }


type alias StartPosition =
    { x : Float, y : Float }


polygonSnapDistance =
    4



--Information required to create a Svg rectangle


type alias RectangleAttributes =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    , rx : Float
    , ry : Float
    }



--Information required to create a Svg Elipse


type alias ElipseAttributes =
    { cx : Float
    , cy : Float
    , rx : Float
    , ry : Float
    }



--Attribute information shared by different Svg shape types


type alias CommonAttributes =
    { stroke : String
    , strokeWidth : String
    , fill : String

    --, transform : Transform
    }



--System for applying transform info for shape editing


type Transform
    = None
    | Translate Float Float {- X, Y -} Transform
    | Scale Float Float {- X, Y -} Transform
    | Rotate Float Float Float {- degrees, X, Y -} Transform
    | SkewX Float {- degrees -} Transform
    | SkewY Float {- degrees -} Transform



--helper functions


toSvg : List (Svg.Attribute msg) -> Graphic -> Svg.Svg msg



--converts Graphic type data to Svg Msg type data


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

        Polygon startPoint pointList common ->
            Svg.polygon ([ SvgA.points <| pointListToString (startPoint :: pointList) ] ++ commonToSvgA common ++ extraAttributes) []


pointListToString pList =
    case pList of
        [] ->
            ""

        --this branch is for safety (covers all cases) in case the function is used incorrectly
        --Since the Polygon Graphic type requires a start point, this list should never be empty
        { x, y } :: [] ->
            toString x ++ " " ++ toString y

        { x, y } :: ps ->
            toString x ++ " " ++ toString y ++ ", " ++ pointListToString ps


commonToSvgA : CommonAttributes -> List (Svg.Attribute msg)
commonToSvgA common =
    [ SvgA.fill common.fill
    , SvgA.stroke common.stroke
    , SvgA.strokeWidth common.strokeWidth

    --   , SvgA.transform <| toString common.transform
    ]



--Constructor functions for the opaque Graphic type


createRectangle : RectangleAttributes -> CommonAttributes -> Graphic
createRectangle rec common =
    Rectangle rec common


createElipse : ElipseAttributes -> CommonAttributes -> Graphic
createElipse rec common =
    Elipse rec common


createPolygon polyList common =
    Polygon polyList common
