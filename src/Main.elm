module Main exposing (main)

import Browser
import Color
import Html exposing (Html)
import List
import TypedSvg exposing (circle, g, line, svg)
import TypedSvg.Attributes exposing (fill, stroke, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, strokeWidth, width, x1, x2, y1, y2)
import TypedSvg.Types exposing (Fill(..))


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
    , width : Float
    , height : Float
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
                List.map createEdges
                    (case firstLayer of
                        Nothing ->
                            []

                        Just layer ->
                            layer
                    )
                    :: connectNodes
                        (case List.tail net of
                            Nothing ->
                                []

                            Just tail ->
                                tail
                        )
    in
    { net = connectNodes net_
    , nodeRadius = 40
    , width = 600
    , height = 600
    }


type Msg
    = Step Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        Step num ->
            model


view : Model -> Html Msg
view model =
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
        [ width model.width
        , height model.height
        , viewBox 0 0 model.width model.height
        ]
        (List.map displayLayer model.net)


grey : Float -> Color.Color
grey scale =
    let
        value =
            1 - scale
    in
    Color.rgb value value value


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
