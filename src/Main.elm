module Main exposing (main)

import Browser
import Color
import Html exposing (Html)
import List
import TypedSvg exposing (circle, g, line, svg)
import TypedSvg.Attributes exposing (fill, stroke, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, strokeWidth, width, x1, x2, y1, y2)
import TypedSvg.Types exposing (Fill(..))
import Random

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
    , layers : List Int
    , width : Float
    , height : Float
    , nodeRadius : Float
    , edgeWidth: Float
    }


initialModel : Model
initialModel =
    let
        width_ =
            1500.0

        height_ =
            900.0

        layers_ =
            [ 3
            , 5
            , 5
            , 3
            , 2
            ]

        spacingX =
            width_ / toFloat (List.length layers_ + 1)

        nodeRadius_ =
            if List.any (\size -> size > 16) layers_ then
                10
            else if List.any (\size -> size > 8) layers_ then
                25
            else
                40

        edgeWidth_ =
            if List.any (\size -> size > 16) layers_ then
                1
            else if List.any (\size -> size > 8) layers_ then
                2
            else
                3

        initialSeed_ =
            Random.initialSeed 47

        generateRandomNumbers : Random.Seed -> Int -> (List Float, Random.Seed)
        generateRandomNumbers seed times =
            let 
                (num, nextSeed) = Random.step (Random.float 0.1 1) seed
                -- _ = Debug.log "num" num
            in
            if times <= 0 then
                ([], nextSeed)
            else
                let
                    (rests, finalSeed) = generateRandomNumbers nextSeed (times - 1)
                in
                (num :: rests, finalSeed)


        createLayer : Int -> Random.Seed -> List Int -> Int -> Int -> List Node
        createLayer nodeCount seed layers layerIndex firstLength =
            let
                spacingY =
                    height_ / toFloat (firstLength + 1)

                (randomNumbers, nextSeed) =
                    generateRandomNumbers seed (secondLength + 1)
                
                -- temporary placeholder values
                -- fetch these from server
                activation =
                    case List.head randomNumbers of
                        Nothing ->
                            1
                        Just num ->
                            num
                secondLength =
                    case List.head (List.drop (layerIndex + 1) layers) of
                        Nothing ->
                            0

                        Just length ->
                            length

                weights =
                    case List.tail randomNumbers of
                        Nothing ->
                            []
                        Just nums ->
                            nums

                x =
                    toFloat (layerIndex + 1) * spacingX

            in
            if nodeCount <= 0 then
                []

            else
                Node x (spacingY * toFloat nodeCount) activation weights []
                    :: createLayer (nodeCount - 1) nextSeed layers layerIndex firstLength

        net_ =
            -- [ [ Node 100 300 0.2 [ 0.2, 0.6, 0.87, 0.5] [] ]
            -- , [ Node 300 100 0.238 [ 0.87, 0.5, 0.2, 0.3] []
            --   , Node 300 300 0.8 [ 0.87, 0.5, 0.2, 0.3] []
            --   , Node 300 400 0.8 [ 0.87, 0.5, 0.2, 0.3] []
            --   , Node 300 500 0.8 [ 0.87, 0.5, 0.2, 0.3] []
            --   ]
            -- , [ Node 500 300 0.6 [] []
            --   , Node 500 400 0.6 [] []
            --   ]
            -- ]
            List.indexedMap
                (\layerIndex firstLength ->
                    createLayer firstLength initialSeed_ layers_ layerIndex firstLength
                )
                layers_
    
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
    , layers = layers_
    , nodeRadius = nodeRadius_
    , edgeWidth = edgeWidth_
    , width = width_
    , height = height_
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
                        , strokeWidth model.edgeWidth
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
