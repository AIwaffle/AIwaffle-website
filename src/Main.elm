module Main exposing (main)

import Browser
import Color
import Html exposing (Html)
import Html.Attributes
import List
import List.Extra
import Random
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Line exposing (..)
import Canvas.Settings.Text exposing (..)
import Round
import Element as E
import Element.Input as Input
import Element.Background as Background
import Element.Border as Border

type alias Node =
    { x : Float
    , y : Float
    , activation : Float
    , weights : List Float
    }


type alias Layer =
    List Node


type alias Net =
    List Layer


type alias Model =
    { net : Net
    , layers : List Int
    , width : Int
    , height : Int
    , nodeRadius : Float
    , edgeWidth: Float
    , learningRate: Float
    , losses : List Float
    }

main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


initialModel : Model
initialModel =
    let
        width_ =
            1300

        height_ =
            700

        layers_ =
            [ 2
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

        (losses_, _) =
            generateRandomNumbers
            initialSeed_
            (case List.Extra.last layers_ of
                Nothing ->
                    0
                Just n ->
                    n
            )
        
        

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

                -- add 1 to prevLength for the node's activation in addition to weights
                (randomNumbers, nextSeed) =
                    generateRandomNumbers seed (prevLength + 1)
                
                -- temporary placeholder values
                -- fetch these from server
                activation =
                    case List.head randomNumbers of
                        Nothing ->
                            1
                        Just num ->
                            num
                prevLength =
                    case List.head (List.drop (layerIndex - 1) layers) of
                        Nothing ->
                            0

                        Just length ->
                            length

                _ = Debug.log "prevLength" prevLength

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
                Node x (spacingY * toFloat nodeCount) activation weights
                    :: createLayer (nodeCount - 1) nextSeed layers layerIndex firstLength

        net_ =
            List.indexedMap
                (\layerIndex firstLength ->
                    createLayer firstLength initialSeed_ layers_ layerIndex firstLength
                )
                layers_
    
    in
    { net = net_
    , layers = layers_
    , nodeRadius = nodeRadius_
    , edgeWidth = edgeWidth_
    , width = width_
    , height = height_
    , learningRate = 0.5
    , losses = losses_
    }


type Msg
    = AdjustLearningRate Float
    | Step Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        AdjustLearningRate rate ->
            { model | learningRate = rate }
        Step num ->
            model


neuralNet : Model -> Html Msg
neuralNet model =
    let
        displayLayerEdges : Layer -> Layer -> List Renderable
        displayLayerEdges prevLayer currLayer =
            flatten2D (List.map (displayEdges prevLayer) currLayer)

        displayLayerNodes : Layer -> Layer -> List Renderable
        displayLayerNodes prevLayer currLayer =
            flatten2D (List.map displayNode currLayer)
            

        displayNode node =
            [ shapes
                [ fill (greyScale node.activation)
                , stroke Color.black
                ]
                [ circle
                    (node.x, node.y) model.nodeRadius
                ]
            , text
                    [ font 
                        { size = round (model.nodeRadius * 0.8)
                        , family = "sans-serif"
                        }
                    , align Center
                    , baseLine Middle
                    , fill (highContract node.activation)
                    ]
                    (node.x, node.y)
                    (Round.round 2 node.activation)
            ]

        displayEdges : Layer -> Node -> List Renderable
        displayEdges prevLayer node =
            let
                _ = Debug.log "prevLayer" prevLayer
            in
            List.map2
                (displayEdge node)
                prevLayer
                node.weights

        displayEdge : Node -> Node -> Float -> Renderable
        displayEdge start end weight =
            shapes
            [ stroke (greyScale weight)
            , lineWidth model.edgeWidth
            ]
            [ path (start.x, start.y)
                [ lineTo (end.x, end.y)
                ]
            ]
        

        displayLosses : List (List Renderable)
        displayLosses =
            let
                width =
                    model.nodeRadius * 2
                height =
                    model.nodeRadius * 2
            in
            List.map2
            (\node loss ->
                [ shapes
                    [ stroke Color.red
                    , lineWidth model.edgeWidth
                    ]
                    [ rect (node.x + width, node.y - height / 2) width height
                    ]
                , text
                    [ font 
                        { size = round (model.nodeRadius * 0.8)
                        , family = "sans-serif"
                        }
                    , align Center
                    , baseLine Middle
                    , fill Color.red
                    ]
                    (node.x + width * 3/2, node.y)
                    (Round.round 2 loss)
                ]
            )
            (case List.Extra.last model.net of
                Nothing ->
                    []
                Just layer ->
                    layer
            )
            model.losses
        
        displayLayers func =
            flatten2D (List.map2 func
            (case List.Extra.init model.net of
                Just init ->
                    [] :: init
                Nothing ->
                    [[]]
            )
            model.net
            )

    in
    Canvas.toHtml ( model.width, model.height )
        []
        (displayLayers displayLayerEdges
        ++ displayLayers displayLayerNodes
        ++ flatten2D displayLosses)


controls : Model -> E.Element Msg
controls model =
    E.row
    [ E.spacing 20   
    ]
    [ learningRateControl model
    , stepControl model
    ]


learningRateControl : Model -> E.Element Msg
learningRateControl model =
    Input.slider
        [ E.height (E.px 10)
        , E.width (E.px 180)
        , E.behindContent
            (E.el
                [ E.width E.fill
                , E.height (E.px 10)
                , E.centerX
                , Background.color grey
                , Border.rounded 5
                ]
                E.none
            )
        ]
        { min = 0
        , max = 5
        , step = Nothing
        , value = model.learningRate
        , thumb = Input.defaultThumb
        , label =
            Input.labelAbove
                [ E.paddingEach
                    { top = 0
                    , bottom = 10
                    , left = 0
                    , right = 0
                    }
                ]
                (E.text ("Learning Rate: " ++ Round.round 2 model.learningRate))
        , onChange = AdjustLearningRate
        }


stepControl : Model -> E.Element Msg
stepControl model =
    Input.button
        [ Background.color lightGrey
        , E.mouseOver
            [ Background.color grey ]
        , E.padding 10
        , Border.rounded 5
        ]
        { onPress = Just (Step 1)
        , label = E.text "Move 1 Step"
        }


view : Model -> Html Msg
view model =
    E.layout
    []
    <|
        E.column
        [ E.width E.fill
        ]
        [ center
          (E.html (neuralNet model))
        , center
          (controls model)
        ]


center : E.Element msg -> E.Element msg
center element =
    E.el
        [ E.htmlAttribute (Html.Attributes.style "margin" "auto")
        ]
        element


lightGrey : E.Color
lightGrey = E.rgb 0.8 0.8 0.8


grey : E.Color
grey = E.rgb 0.6 0.6 0.6


darkGrey : E.Color
darkGrey = E.rgb 0.4 0.4 0.4


greyScale : Float -> Color.Color
greyScale scale =
    let
        value =
            1 - scale
    in
    Color.rgb value value value

highContract :  Float -> Color.Color
highContract scale =
    greyScale (if scale < 0.5 then 1 else 0 )

-- source: https://gist.github.com/maticzav/f0b9177bf59d3efa44815167fd55cdf0
flatten2D : List (List a) -> List a
flatten2D list =
  List.foldr (++) [] list
