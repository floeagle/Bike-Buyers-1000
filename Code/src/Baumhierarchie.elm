module Baumhierarchie exposing (..)

import Html exposing (Html, button, div, text)
import Http
import Json.Decode
import TreeDiagram
import TreeDiagram.Svg
import TypedSvg exposing (circle, g, line, path,style, svg, text_)
import Browser
import Color
import TypedSvg.Attributes exposing (class, d, fill, fontFamily, fontSize, stroke, strokeWidth, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- aus übungsvorlage
type alias Model =
    { tree : TreeDiagram.Tree String, errorMsg : String }


treeDecoder : Json.Decode.Decoder (TreeDiagram.Tree String)
treeDecoder =
    Json.Decode.map2
        (\name children ->
            case children of
                Nothing ->
                    TreeDiagram.node name []

                Just c ->
                    TreeDiagram.node name c
        )
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.maybe <|
            Json.Decode.field "children" <|
                Json.Decode.list <|
                    Json.Decode.lazy
                        (\_ -> treeDecoder)
        )

 -- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none       