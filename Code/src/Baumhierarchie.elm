module Baumhierarchie exposing (..)

import Html exposing (Html, button, div, text)
import Http
import Json.Decode
import TreeDiagram exposing (TreeLayout, topToBottom)
import TreeDiagram.Svg
import TypedSvg exposing (circle, g, line, path,style, svg, text_)
import Browser
import Color
import TypedSvg.Attributes exposing (class, d, fill, fontFamily, fontSize, stroke, strokeWidth, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))


type alias Model =
    { tree : TreeDiagram.Tree String, errorMsg : String }
type Msg
    = GotTree (Result Http.Error (TreeDiagram.Tree String))

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }





init : () -> ( Model, Cmd Msg )
init () =
    ( { tree = TreeDiagram.node "" [], errorMsg = "Loading" }
    , Http.get { url = "https://raw.githubusercontent.com/floeagle/Bike-Buyers-1000/main/Daten-zum-Laden/Datenvorverarbeitung/JSON/DatenvorverarbeitungohneCarWorldwide.json"
    , expect = Http.expectJson GotTree treeDecoder }
    )



subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none




update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTree (Ok newTree) ->
            ( { model | tree = newTree, errorMsg = "No Error" }, Cmd.none )

        GotTree (Err error) ->
            ( { model
                | tree = TreeDiagram.node "" []
                , errorMsg =
                    case error of
                        Http.BadBody newErrorMsg ->
                            newErrorMsg

                        _ ->
                            "Some other Error"
              }
            , Cmd.none
            )


drawLine : ( Float, Float ) -> Svg msg
drawLine ( targetX, targetY ) =
    line
        [ x1 0
        , y1 0
        , x2 targetX
        , y2 targetY
        , stroke (TypedSvg.Types.Paint Color.lightBlue) 
        ]
        []



drawNode : String -> Svg msg
drawNode str =
    g
        []
        [ circle 
            [ r 16
            , stroke (Paint Color.black)
            , fill (Paint Color.blue)
            , cx 0
            , cy 0 
            ] 
            []
        , text_ 
            [ textAnchor AnchorStart
            , transform 
                [ Translate 0.5 17
                , Rotate 90.0 0.0 0.0
                ]
            ] 
            [ text str ]
        ]

tLayout : TreeLayout
tLayout =
    TreeLayout topToBottom 
            250 
            20
            600
            300

view : Model -> Html Msg
view model =
    div []
        [ TreeDiagram.Svg.draw tLayout drawNode drawLine model.tree 
        ]
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
        (Json.Decode.field "data" (Json.Decode.field "id" Json.Decode.string))
        (Json.Decode.maybe <|
            Json.Decode.field "children" <|
                Json.Decode.list <|
                    Json.Decode.lazy
                        (\_ -> treeDecoder)
        )
