module Main exposing (main)

import Browser
import Color
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List
import TypedSvg exposing (circle, g, line, svg)
import TypedSvg.Attributes exposing (fill, stroke, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, strokeWidth, width, x1, x2, y1, y2)
import TypedSvg.Types exposing (Fill(..), px)


type alias Node =
    { x : Float
    , y : Float
    , activation : Float
    , weights : List Float
    , edges : List Edge
    }


type Edge
    = Edge
        { start : Node
        , end : Node
        , weight : Float
        }


type alias Net =
    List (List Node)


type alias Model =
    { net : Net
    , nodeRadius : Float
    }


initialModel : Model
initialModel =
    let
        net_ =
            [ [ Node 100 300 0.2 [ 0.2, 0.6 ] [] ]
            , [ Node 300 200 0.238 [ 0.87 ] []
              , Node 300 400 0.8 [ 0.9 ] []
              ]
            , [ Node 500 300 0.6 [] [] ]
            ]

        connectNodes : Net -> Net
        connectNodes net =
            let
                firstLayer =
                    List.head net

                secondLayer =
                    List.head
                        (case List.tail net of
                            Nothing ->
                                []

                            Just nodes ->
                                nodes
                        )

                connect : Node -> Float -> Node -> Edge
                connect start weight end =
                    Edge
                        { start = start
                        , end = end
                        , weight = weight
                        }

                createEdges : Node -> Node
                createEdges start =
                    case secondLayer of
                        Nothing ->
                            start

                        Just layer ->
                            { start
                                | edges = List.map2 (connect start) start.weights layer
                            }
            in
            if List.length net <= 1 then
                net

            else
                [ List.map createEdges
                    (case firstLayer of
                        Nothing ->
                            []

                        Just layer ->
                            layer
                    )
                ]
                    ++ connectNodes
                        (case List.tail net of
                            Nothing ->
                                []

                            Just tail ->
                                tail
                        )
    in
    { net = connectNodes net_
    , nodeRadius = 40
    }


type Msg
    = NewActivation Float


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewActivation f ->
            model


view : Model -> Html Msg
view model =
    {- svg
        [ width 600
        , height 600
        , viewBox 0 0 600 600
        ]
        [ circle [ cx 100, cy 300, r 40 ] []
       , circle [ cx 300, cy 200, r 40 ] []
       , circle [ cx 300, cy 400, r 40 ] []
       , circle [ cx 500, cy 300, r 40 ] []
       , line [x1 100, y1 300, x2 300, y2 200, stroke Color.black, strokeWidth 5 ] []
       , line [x1 100, y1 300, x2 300, y2 400, stroke Color.black, strokeWidth 5 ] []
       , line [x1 500, y1 300, x2 300, y2 200, stroke Color.black, strokeWidth 5 ] []
       , line [x1 500, y1 300, x2 300, y2 400, stroke Color.black, strokeWidth 5 ] []
       ]
    -}
    let
        displayLayer : List Node -> Html Msg
        displayLayer layer =
            g
                []
                (List.map displayEdges layer
                    ++ List.map displayNode layer
                )

        displayNode node =
            g
                []
                [ circle
                    [ cx node.x
                    , cy node.y
                    , r model.nodeRadius
                    , fill
                        (Fill <| grey node.activation)
                    ]
                    []
                ]

        displayEdges node =
            g
                []
                (List.map displayEdge node.edges)

        displayEdge : Edge -> Html Msg
        displayEdge edge =
            case edge of
                Edge { start, end, weight } ->
                    line
                        [ x1 start.x
                        , y1 start.y
                        , x2 end.x
                        , y2 end.y
                        , strokeWidth 5
                        , stroke (grey weight)
                        ]
                        []
    in
    svg
        [ width 600
        , height 600
        , viewBox 0 0 600 600
        ]
        (List.map displayLayer model.net)


grey : Float -> Color.Color
grey scale =
    let
        value = 1 - scale
    in
    Color.rgb value value value


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
