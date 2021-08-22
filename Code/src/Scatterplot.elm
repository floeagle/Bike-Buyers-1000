module Scatterplot exposing (..)

import Axis
import Statistics
import Html exposing (Html)
import Scale exposing (ContinuousScale)
import TypedSvg exposing (circle, g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Transform(..))
import Csv.Decode as Decode exposing (Decoder)


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    60


radius : Float
radius =
    5.0


tickCount : Int
tickCount =
    5


defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )

xScale : List Float -> ContinuousScale Float
xScale values =
    Scale.linear ( 0, w - 2 * padding ) ( wideExtent values )


yScale : List Float -> ContinuousScale Float
yScale values =
    Scale.linear ( h - 2 * padding, 0 ) ( wideExtent values )

wideExtent : List Float -> ( Float, Float )
wideExtent values =
    let
        scale: (Float, Float)
        scale = Maybe.withDefault defaultExtent (Statistics.extent values)

        range: Float
        range = -(Tuple.first scale) + (Tuple.second scale)

        down: Float
        down = (Tuple.first scale) - range/(Basics.toFloat (2*tickCount))

        up: Float
        up = (Tuple.second scale)+range/(Basics.toFloat (2*tickCount))
    in         
        if (down < 0) then 
            (0, up)
        else
            (down, up)


xAxis : List Float -> Svg msg
xAxis values =
    Axis.bottom [ Axis.tickCount tickCount ] (xScale values)


yAxis : List Float -> Svg msg
yAxis values =
    Axis.left [ Axis.tickCount tickCount ] (yScale values)


main : Html msg
main =
            
            Html.main_ []
                [ Html.h1 [] [ Html.text "Bike_Buyers" ]]