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
import Markdown

type alias Node =
    { x : Float
    , y : Float
    , pos : Position
    , activation : Float
    , weights : List Float
    }


type alias Position =
    (Int, Int)


type MoveDirection
    = Forward
    | Backward


type alias Layer =
    List Node


type alias Net =
    List Layer


type alias Weights =
    List (List (List Float))


type alias Activations =
    List (List Float)


type alias Losses =
    List Float


type alias Model =
    { net : Net
    , nextNet : Net
    , layers : List Int
    , width : Int
    , height : Int
    , nodeRadius : Float
    , edgeWidth: Float
    , learningRate: Float
    , losses : List Float
    , currentPosition : Position
    , currentDirection : MoveDirection
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
            900

        height_ =
            620

        layers_ =
            -- [ 2
            -- , 3
            -- , 2
            -- ]
            -- [ 3
            -- , 5
            -- , 3
            -- ]
            [ 10
            , 20
            , 20
            , 10
            ]

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
            47

        (nextNet_, losses_) =
            generateRandomNet layers_ height_ width_ initialSeed_ generateAllLayerValues
    
        net_ =
            clearActivations nextNet_

    in
    { net = net_
    , nextNet = nextNet_
    , layers = layers_
    , nodeRadius = nodeRadius_
    , edgeWidth = edgeWidth_
    , width = width_
    , height = height_
    , learningRate = 0.5
    , losses = losses_
    -- start from the first input node
    , currentPosition = (0, 0)
    -- start with forward propgation
    , currentDirection = Forward
    }


clearActivations : Net -> Net
clearActivations net =
    List.map
        (\layer ->
            List.map 
            (\node -> { node | activation = 0 })
            layer
        )
        net


generateAllLayerWeights : Int -> Random.Seed -> Int -> List Int -> (List Float, List (List Float))
generateAllLayerWeights nodeCount seed layerIndex layers =
    let
        activation =
            0

        prevLength =
            case List.head (List.drop (layerIndex - 1) layers) of
                Nothing ->
                    0

                Just length ->
                    length

        (weights, nextSeed) =
            generateRandomNumbers seed -5.0 5.0 prevLength

    in
    if nodeCount <= 0 then
        ([], [])

    else
        let
            (nextActivation, nextWeights) =
                generateAllLayerWeights (nodeCount - 1) nextSeed layerIndex layers
        in
        (activation :: nextActivation, weights :: nextWeights)


generateAllLayerValues : Int -> Random.Seed -> Int -> List Int -> (List Float, List (List Float))
generateAllLayerValues nodeCount seed layerIndex layers =
    let
        (activation, seed1) =
            generateRandomNumber seed 0.1 1

        prevLength =
            case List.head (List.drop (layerIndex - 1) layers) of
                Nothing ->
                    0

                Just length ->
                    length

        (weights, nextSeed) =
            generateRandomNumbers seed1 -5.0 5.0 (prevLength)

    in
    if nodeCount <= 0 then
        ([], [])

    else
        let
            (nextActivation, nextWeights) =
                generateAllLayerValues (nodeCount - 1) nextSeed layerIndex layers
        in
        (activation :: nextActivation, weights :: nextWeights)


generateRandomNumbers : Random.Seed -> Float -> Float -> Int -> (List Float, Random.Seed)
generateRandomNumbers seed min max times =
    let 
        (num, nextSeed) = generateRandomNumber seed min max
    in
    if times <= 0 then
        ([], nextSeed)
    else
        let
            (rests, finalSeed) = generateRandomNumbers nextSeed min max (times - 1)
        in
        (num :: rests, finalSeed)


generateRandomNumber : Random.Seed -> Float -> Float -> (Float, Random.Seed)
generateRandomNumber seed min max =
    Random.step (Random.float min max) seed


generateRandomNet : List Int -> Int -> Int -> Int -> (Int -> Random.Seed -> Int -> List Int -> (List Float, List (List Float))) -> (Net, List Float)
generateRandomNet layers height width seed generateLayerValues =
    let
        initialSeed =
            Random.initialSeed seed


        (netLosses, _) =
            generateRandomNumbers
                initialSeed
                0.1
                1.0
                (case List.Extra.last layers of
                    Nothing ->
                        0
                    Just n ->
                        n
                )
        

        (netActivations, netWeights) =
            List.Extra.indexedFoldr
                (\layerIndex layerLength values ->
                    let
                        (layerActivations, layerWeights) =
                            generateLayerValues layerLength initialSeed layerIndex layers

                        activations =
                            Tuple.first values

                        weights =
                            Tuple.second values                        

                    in
                    (layerActivations :: activations, layerWeights :: weights)
                )
                ([], [])
                layers

    in
    generateNet layers height width netActivations netWeights netLosses


generateNet : List Int -> Int -> Int -> Activations -> Weights -> Losses -> (Net, List Float)
generateNet layers height width activations weights losses =
    let
        spacingX =
            toFloat width / toFloat (List.length layers)

        createLayer : Int -> Int -> Int -> List Float -> List (List Float) -> List Node
        createLayer nodeCount layerIndex layerLength layerActivations layerWeights =
            let
                nodeIndex =
                    nodeCount - 1

                spacingY =
                    toFloat height / toFloat (layerLength + 1)

                sizeMargin = 100

                x =
                    if layerIndex == 0 then
                        sizeMargin
                    else
                        sizeMargin + toFloat (layerIndex) * spacingX

                nodeActivation =
                    Maybe.withDefault 0 (nth nodeIndex layerActivations)

                nodeWeights =
                    Maybe.withDefault [] (nth nodeIndex layerWeights)

            in
            if nodeCount <= 0 then
                []

            else
                createLayer (nodeCount - 1) layerIndex layerLength layerActivations layerWeights
                    ++ [Node x (spacingY * toFloat nodeCount) (layerIndex, nodeIndex) nodeActivation nodeWeights]

        net =
            List.indexedMap
                (\layerIndex layerLength ->
                    createLayer layerLength layerIndex layerLength
                        (Maybe.withDefault [] (nth layerIndex activations))
                        (Maybe.withDefault [] (nth layerIndex weights))
                )
                layers
    in
    (net, losses)


type Msg
    = AdjustLearningRate Float
    | MoveOneStep
    | MoveOneLayer


update : Msg -> Model -> Model
update msg model =
    let
        _ =
            Debug.log "currentDirection" model.currentDirection
    in
    case msg of
        AdjustLearningRate rate ->
            { model | learningRate = rate }
        MoveOneStep ->
            case model.currentDirection of
                Forward ->
                    forwardOneStep model
                Backward ->
                    backwardOneStep model
        MoveOneLayer ->
            case model.currentDirection of
                Forward ->
                    forwardOneLayer model
                Backward ->
                    backwardOneLayer model


forwardOneLayer : Model -> Model
forwardOneLayer model =
    let
        currentLayerIndex =
            Tuple.first model.currentPosition
        currentIndex =
            Tuple.second model.currentPosition
        currentLayerLength =
            Maybe.withDefault 0 (nth currentLayerIndex model.layers)
    in
    if currentIndex < currentLayerLength - 1 then
        repeat (currentLayerLength - currentIndex) forwardOneStep model
    else if currentIndex == currentLayerLength then
        forwardOneLayer (forwardOneStep model)
    else
        model


backwardOneLayer : Model -> Model
backwardOneLayer model =
    let
        currentLayerIndex =
            Tuple.first model.currentPosition
        currentIndex =
            Tuple.second model.currentPosition
    in
    if currentIndex > 0 then
        repeat currentIndex backwardOneStep model
    else if currentIndex == 0 then
        if currentLayerIndex == 0 then
            model
        else
            backwardOneLayer (backwardOneStep model)
    else
        model


forwardOneStep : Model -> Model
forwardOneStep model =
    let
        layerLength =
            case nth currentLayerIndex model.layers of
                Nothing ->
                    0
                Just n ->
                    n
        numberOfLayers =
            List.length model.layers
        currentLayerIndex =
            Tuple.first model.currentPosition 
        _ =
            Debug.log "currentLayerIndex" currentLayerIndex
        currentIndex =
            Tuple.second model.currentPosition
        _ =
            Debug.log "currentIndex" currentIndex
        nextNet =
            updateActivations currentLayerIndex currentIndex model.net model.nextNet
    in
    if currentIndex >= layerLength then
        if currentLayerIndex >= numberOfLayers - 1 then
            { model
                | currentPosition = (numberOfLayers - 1, layerLength - 1)
                , currentDirection = Backward
                , nextNet = Tuple.first (generateRandomNet model.layers model.height model.width 128 generateAllLayerValues)
            }
        else
            { model
                | currentPosition = (currentLayerIndex + 1, 0)
                , net = nextNet
            }
    else
        { model
            | currentPosition = (currentLayerIndex, currentIndex + 1)
            , net = nextNet
        }


backwardOneStep : Model -> Model
backwardOneStep model =
    let
        currentLayerIndex =
            Tuple.first model.currentPosition
        currentIndex =
            Tuple.second model.currentPosition
        _ =
            Debug.log "currentLayerIndex" currentLayerIndex
        _ =
            Debug.log "currentIndex" currentIndex
        nextLayerLength =
            Maybe.withDefault 0 (nth (currentLayerIndex - 1) model.layers)
        nextNet =
            updateWeights currentLayerIndex currentIndex model.net model.nextNet
    in
    if currentIndex <= 0 then
        if currentLayerIndex <= 1 then
            { model
                | currentPosition = (0, 0)
                , currentDirection = Forward
                , net = clearActivations model.nextNet
            }
        else
            { model
                | currentPosition = (currentLayerIndex - 1, nextLayerLength - 1)
                , net = nextNet
            }
    else
        { model
            | currentPosition = (currentLayerIndex, currentIndex - 1)
            , net = nextNet
        }


emptyNode : Node
emptyNode =
    { x = 0
    , y = 0
    , pos = (0, 0)
    , activation = 0
    , weights = []
    }


updateActivations : Int -> Int -> Net -> Net -> Net
updateActivations layerIndex index currNet nextNet =
    let
        combineNodes currNode nextNode =
            { currNode | activation = nextNode.activation }
    in
    updateNode layerIndex index combineNodes currNet nextNet


updateWeights : Int -> Int -> Net -> Net -> Net
updateWeights layerIndex index currNet nextNet =
    let
        combineNodes currNode nextNode =
            { currNode | weights = nextNode.weights }
    in
    updateNode layerIndex index combineNodes currNet nextNet


updateNode : Int -> Int -> (Node -> Node -> Node) -> Net -> Net -> Net
updateNode layerIndex index combineNodes currNet nextNet =
    let
        -- _ =
        --     Debug.log "layerIndex" layerIndex
        -- _ =
        --     Debug.log "index" index
        -- _ =
        --     Debug.log "currLayer" currLayer
        currLayer =
            Maybe.withDefault [] (nth layerIndex currNet)
        currNode =
            Maybe.withDefault emptyNode (nth index currLayer)
        nextLayer =
            Maybe.withDefault [] (nth layerIndex nextNet)
        nextNode =
            Maybe.withDefault emptyNode (nth index nextLayer)
    in
    List.Extra.setAt
        layerIndex
        (List.Extra.setAt
            index
            (combineNodes currNode nextNode)
            currLayer
        )
        currNet



neuralNet : Model -> Html Msg
neuralNet model =
    let
        displayLayerEdges : Layer -> Layer -> List Renderable
        displayLayerEdges prevLayer currLayer =
            flatten2D (List.map (displayEdges prevLayer) currLayer)

        displayLayerNodes : Layer -> Layer -> List Renderable
        displayLayerNodes prevLayer currLayer =
            flatten2D (List.map displayNode currLayer)
            

        isVisitedNode : Node -> Position -> Bool
        isVisitedNode node currentPosition =
            let
                nodeLayerIndex =
                    Tuple.first node.pos
                nodeIndex =
                    Tuple.second node.pos
                currentLayerIndex =
                    Tuple.first currentPosition
                currentIndex =
                    Tuple.second currentPosition
            in
            case model.currentDirection of
                Forward ->
                    nodeLayerIndex < currentLayerIndex
                    || (nodeLayerIndex == currentLayerIndex && nodeIndex <= currentIndex)
                Backward ->
                    nodeLayerIndex > currentLayerIndex
                    || (nodeLayerIndex == currentLayerIndex && nodeIndex >= currentIndex)


        isCurrentNode : Node -> Position -> Bool
        isCurrentNode node currentPosition =
            node.pos == currentPosition


        displayNode node =
            [ shapes
                [ fill
                    (if isVisitedNode node model.currentPosition then
                        greenScale node.activation
                    else
                        greyScale node.activation
                    )
                , stroke
                    (if isVisitedNode node model.currentPosition then
                        Color.yellow
                    else
                        Color.black
                    )
                , lineWidth (model.nodeRadius * 0.10)
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
            List.map2
                (displayEdge node)
                prevLayer
                node.weights

        displayEdge : Node -> Node -> Float -> Renderable
        displayEdge start end weight =
            shapes
            [ stroke
                ( if isVisitedNode start model.currentPosition then
                    greenScale weight
                else
                    greyScale weight
                )
            , lineWidth
                (if isCurrentNode start model.currentPosition then
                    model.edgeWidth * 2
                else
                    model.edgeWidth
                )
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


        clearBackground : List Renderable
        clearBackground =
            [ shapes
                [ fill Color.white ]
                [ rect (0, 0) (toFloat model.width) (toFloat model.height)
                ]
            ]
        

    in
    Canvas.toHtml ( model.width, model.height )
        []
        (clearBackground
        ++ displayLayers displayLayerEdges
        ++ displayLayers displayLayerNodes
        ++ flatten2D displayLosses)


controls : Model -> E.Element Msg
controls model =
    E.row
    [ E.spacing 20   
    ]
    [ learningRateControl model
    , stepControl
    , layerStepControl
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


stepControl : E.Element Msg
stepControl =
    controlButton
        { onPress = Just MoveOneStep
        , label = E.text "Move 1 Step"
        }


layerStepControl : E.Element Msg
layerStepControl =
    controlButton
        { onPress = Just MoveOneLayer
        , label = E.text "Move 1 Layer"
        }


controlButton : { label : E.Element msg, onPress : Maybe msg } -> E.Element msg
controlButton =
    Input.button
        [ Background.color lightGrey
        , E.mouseOver
            [ Background.color grey ]
        , E.padding 10
        , Border.rounded 5
        ]


directionTracker : Model -> E.Element Msg
directionTracker model =
    let
        direction =
            case model.currentDirection of
                Forward ->
                    "forward"
                Backward ->
                    "backward"
        background =
            let
                lastLayerIndex =
                    List.length model.layers - 1
                lastIndex =
                    Maybe.withDefault 1 (nth lastLayerIndex model.layers) - 1
            in
            case model.currentDirection of
                Forward ->
                    if model.currentPosition == (0, 0) then
                        E.rgba255 51 255 51 0.8
                    else
                        E.rgba255 51 255 51 0.3
                Backward ->
                    if model.currentPosition == (lastLayerIndex, lastIndex) then
                        E.rgba255 255 51 0 0.8
                    else
                        E.rgba255 255 51 0 0.3
    in
    E.el
    [ E.padding 10
    , Background.color background
    ]
    (E.text ("In " ++ direction ++ " propagation"))
    

content : E.Element msg
content =
    E.html (Markdown.toHtml
        [ Html.Attributes.style "white-space" "pre-wrap"
        , Html.Attributes.style "font-size" "0.8em"
        ]
        """

# Introduction

Neural networks and deep learning are big topics in Computer Science and in the technology industry, they currently provide the best solutions to many problems in image recognition, speech recognition and natural language processing. Recently many papers have been published featuring AI that can learn to paint, build 3D Models, create user interfaces(pix2code), some create images given a sentence and there are many more incredible things being done everyday using neural networks.

I’m writing this series of posts about Neural Networks and Deep learning, where I’m going to guide you from learning the basic concepts of Artificial Neural Networks (ANN), show you examples from simple Network to mimic the AND gate, to solving Image recognition tasks using Convolutional Neural Networks (CNNs), Recurrent Neural Networks (RNN) and more. The code will always be written in python, some times with the help of Tensorflow (I don’t expect you to be guru using Tensorflow as I will try to explain the code in details).

# **Agenda**

- **Introduction To Neural Networks (This post)**
- *AND Gate Neural Network (Perceptron) and XOR Gate Feedfoward Neural Network (2 layers).*
- *Mnist Digit Recognition with CNN*
- *Mnist Digit Recognition with RNN*

# Neural Networks

The definition of a neural network, more properly referred to as an 'artificial' neural network (ANN), is provided by the inventor of one of the first neurocomputers, Dr. Robert Hecht-Nielsen. He defines a neural network as:

> "...a computing system made up of a number of simple, highly interconnected processing elements, which process information by their dynamic state response to external inputs."

Or you can also think of Artificial Neural Network as computational model that is inspired by the way biological neural networks in the human brain process information.

# Biological motivation and connections

The basic computational unit of the brain is a **neuron**. Approximately 86 billion neurons can be found in the human nervous system and they are connected with approximately 10¹⁴ — 10¹⁵ **synapses**. The diagram below shows a cartoon drawing of a biological neuron (left) and a common mathematical model (right).

![img](https://miro.medium.com/max/1251/1*Mz0a4EEsdJYsbvf5M_u-Sw.png)

![img](https://miro.medium.com/max/1087/1*Yf6BWJq0kdHTumErO99bUQ.jpeg)

biological neuron (left) and a common mathematical model (right)

The basic unit of computation in a neural network is the neuron , often called a node or unit. It receives input from some other nodes, or from an external source and computes an output. Each input has an associated
weight (w), which is assigned on the basis of its relative importance to other inputs. The node applies a function to the weighted sum of its inputs.

The idea is that the synaptic strengths (the weights *w*) are learnable and control the strength of influence and its direction: excitory (positive weight) or inhibitory (negative weight) of one neuron on another. In the basic model, the dendrites carry the signal to the cell body where they all get summed. If the final sum is above a certain threshold, the neuron can *fire*, sending a spike along its axon. In the computational model, we assume that the precise timings of the spikes do not matter, and that only the frequency of the firing communicates information. we model the *firing rate* of the neuron with an **activation function** *(e.x sigmoid function)*, which represents the frequency of the spikes along the axon.

"""
    )


view : Model -> Html Msg
view model =
    E.layout
    []
    <|
        E.row
        [ E.width E.fill
        , E.height E.fill
        ]
        <|
        [ E.column
            [ E.width (E.fillPortion 3)
            , E.padding 20
            , E.htmlAttribute (Html.Attributes.style "height" "calc(100vh - 20px)")
            , E.htmlAttribute (Html.Attributes.style "overflow-x" "auto")
            , E.htmlAttribute (Html.Attributes.style "overflow-y" "auto")
            ]
            [ content
            ]
        , E.column
            [ E.width (E.fillPortion 5)
            , E.spacing 10
            ]
            ( centerAll
                [ E.html (neuralNet model)
                , directionTracker model
                , controls model
                ]
            )
        ]


centerAll : List (E.Element msg) -> List (E.Element msg)
centerAll elements =
    List.map
    center
    elements


center : E.Element msg -> E.Element msg
center element =
    E.el
        [ E.htmlAttribute (Html.Attributes.style "marginLeft" "auto")
        ,  E.htmlAttribute (Html.Attributes.style "marginRight" "auto")
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
            getColorValue scale 0.4
    in
    Color.rgb value value value


greenScale : Float -> Color.Color
greenScale scale =
    let
        lightness =
            getColorValue scale 0.4
    in
    if scale < 0 then
        Color.hsl 0 0.90 lightness
    else
        Color.hsl 0.3 0.90 lightness


getColorValue : Float -> Float -> Float
getColorValue scale strength =
    let
        compress : Float -> Float
        compress x =
            tanh (strength * x)
        returnValue =
            let
                value = 
                    if scale < 0 then
                        1 + compress scale
                    else
                        1 - compress scale
            in
            if value <= 0.25 then
                value + 0.10
            else
                value
    in
    returnValue


highContract :  Float -> Color.Color
highContract scale =
    let
        value =
            if 1 - scale < 0.5 then
                1
            else
                0
    in
    Color.rgb value value value

-- source: https://gist.github.com/maticzav/f0b9177bf59d3efa44815167fd55cdf0
flatten2D : List (List a) -> List a
flatten2D list =
  List.foldr (++) [] list

nth : Int -> List a -> Maybe a
nth n xs =
    List.head (List.drop n xs)


-- source: https://github.com/elm/core/issues/968
tanh : Float -> Float
tanh x = (e^x - e^ -x) / (e^x + e^ -x)

repeat : Int -> (a -> a) -> a -> a
repeat steps func arg =
    if steps > 0 then
        repeat (steps - 1) func (func arg)
    else
        arg
