port module Page.Tutorial exposing (Model, Msg, init, subscriptions, update, view, getContentName)

import Browser
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Line exposing (..)
import Canvas.Settings.Text exposing (..)
import Color
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import List
import List.Extra
import Random
import Round
import FeatherIcons
import Http
import Json.Encode as Encode
import Json.Decode as Decode

port renderContent : (String -> Cmd msg)

type alias Node =
    { x : Float
    , y : Float
    , pos : Position
    , activation : Float
    , weights : List Float
    }


type alias Position =
    ( Int, Int )


type MoveDirection
    = Forward
    | Backward


type alias Layer =
    List Node


type alias Net =
    List Layer


type alias Floats1 =
    List Float


type alias Floats2 =
    List (List Float)


type alias Floats3 =
    List (List (List Float))

type alias LogisticRegressionModel =
    { x : Floats2
    , y : Floats2
    , w : Floats3
    , a : Floats3
    , loss : Floats1
    }


emptyNet : Net
emptyNet =
    []


type alias Weights =
    List (List (List Float))


type alias Activations =
    List (List Float)


type alias Losses =
    List Float


type alias Model =
    { netIndex : Int
    , net : Net
    , nextNet : Net
    , nets : List Net
    , layers : List Int
    , width : Int
    , height : Int
    , nodeRadius : Float
    , edgeWidth : Float
    , learningRate : Float
    , losses : List Float
    , currentPosition : Position
    , currentDirection : MoveDirection
    , contentIndex : Int
    , demoId : String
    , serverError : Maybe String
    , activationFunction : String
    }

edges :
    { top : Int
    , right : Int
    , bottom : Int
    , left : Int
    }
edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


serverRoot : String
serverRoot =
    "http://106.15.39.117:8080/"


contentNames : List String
contentNames =
    [ "Intro to Machine Learning"
    , "Intro to Deep Learning"
    , "Logistic Regression Model"
    ]


contentDemos : List Bool
contentDemos =
    [ False
    , False
    , True
    ]


firstContentName : String
firstContentName =
    Maybe.withDefault "" <| List.head contentNames


init : String -> ( Model, Cmd Msg )
init contentName =
    let
        width_ =
            900

        height_ =
            620

        layers_ =
            [ 2
            , 1
            ]
            
        nodeRadius_ =
            sizeLevels 10 25 40

        edgeWidth_ =
            sizeLevels 1 2 3

        sizeLevels small medium large =
            if
                List.any (\size -> size > 16) layers_
                    || List.length layers_
                    > 12
            then
                small

            else if
                List.any (\size -> size > 8) layers_
                    || List.length layers_
                    > 8
            then
                medium

            else
                large

        initialSeed_ =
            47

    in
    ( { netIndex = 0
      , net = emptyNet
      , nextNet = emptyNet
      , nets = []
      , layers = layers_
      , nodeRadius = nodeRadius_
      , edgeWidth = edgeWidth_
      , width = width_
      , height = height_
      , learningRate = 0.5
      , losses = []

      -- start from the first input node
      , currentPosition = ( 0, 0 )

      -- start with forward propgation
      , currentDirection = Forward
      , contentIndex = getContentIndex contentName
      , serverError = Nothing
      , demoId = ""
      , activationFunction = "σ"
      }
    , Cmd.batch
        [ renderContent contentName
        , Http.post
            { url = serverRoot ++ "auth/login"
            , body = Http.jsonBody <| Encode.object
                [ ( "username", Encode.string "admin" )
                , ( "password", Encode.string "040506" )
                ]
            , expect = Http.expectWhatever LoggedIn
            }
        ]
    )


clearActivationsExceptFirst : Net -> Net
clearActivationsExceptFirst net =
    updateActivations 0 0 (clearActivations net) net


clearActivations : Net -> Net
clearActivations net =
    List.map
        (\layer ->
            List.map
                (\node -> { node | activation = 0 })
                layer
        )
        net


generateNet : List Int -> Int -> Int -> Activations -> Weights -> Net
generateNet layers height width activations weights =
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

                sideMargin =
                    spacingX / 2

                x =
                    if layerIndex == 0 then
                        sideMargin

                    else
                        sideMargin + toFloat layerIndex * spacingX

                nodeActivation =
                    Maybe.withDefault 0 (nth nodeIndex layerActivations)

                nodeWeights =
                    Maybe.withDefault [] (nth nodeIndex layerWeights)
            in
            if nodeCount <= 0 then
                []

            else
                createLayer (nodeCount - 1) layerIndex layerLength layerActivations layerWeights
                    ++ [ Node x (spacingY * toFloat nodeCount) ( layerIndex, nodeIndex ) nodeActivation nodeWeights ]

        net =
            List.indexedMap
                (\layerIndex layerLength ->
                    createLayer layerLength
                        layerIndex
                        layerLength
                        (Maybe.withDefault [] (nth layerIndex activations))
                        (Maybe.withDefault [] (nth layerIndex weights))
                )
                layers
    in
    net


type Msg
    = AdjustLearningRate Float
    | MoveOneStep
    | MoveOneLayer
    | GetContentFromName String
    | LoggedIn (Result Http.Error ())
    | GetDemoId (Result Http.Error String)
    | GetNextEpoch (Result Http.Error LogisticRegressionModel)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustLearningRate rate ->
            ( { model | learningRate = rate }, Cmd.none )

        MoveOneStep ->
            case model.currentDirection of
                Forward ->
                    forwardOneStep model

                Backward ->
                    ( backwardOneStep model, Cmd.none )

        MoveOneLayer ->
            case model.currentDirection of
                Forward ->
                    forwardOneLayer model

                Backward ->
                    ( backwardOneLayer model, Cmd.none )

        GetContentFromName name ->
            let
                index =
                    getContentIndex name
            in
            ( { model |
                contentIndex = index
            }
            , renderContent name
            )
        
        LoggedIn result ->
            ( case result of
                Err reason ->
                    { model |
                        serverError =
                            Just "Can't log in to server."
                    }
                Ok _ ->
                    model
            , case nth model.contentIndex contentDemos of
                Nothing ->
                    Cmd.none
                Just hasDemo ->
                    if hasDemo then
                        initDemo
                    else
                        Cmd.none
            )
        
        GetDemoId result ->
            case result of
                Ok id ->
                    ({ model |
                        demoId =
                            id
                    }
                    , getEpoch id
                    )
                Err _ ->
                    ({ model |
                        serverError =
                            Just "Can't get demo from server."
                    }
                    , Cmd.none
                    )

        GetNextEpoch result ->
            (case result of
                Ok logisticRegressionModel ->
                    { model |
                        nets =
                            netsFromLogisticRegressionModel model logisticRegressionModel
                    }
                Err _ ->
                    { model |
                        serverError =
                            Just "Can't get next epoch from server."
                    }
            , Cmd.none
            )


netsFromLogisticRegressionModel : Model -> LogisticRegressionModel -> List Net
netsFromLogisticRegressionModel demoModel logisticModel =
    let
        epochNumber =
            List.length logisticModel.loss
        layers =
            [List.length logisticModel.x, List.length logisticModel.y]
        allActivations =
            Maybe.withDefault [] <| nth 0 (Maybe.withDefault [] <| nth 0 logisticModel.a)
        allWeights =
            Maybe.withDefault [] <| nth 0 (Maybe.withDefault [] <| nth 0 logisticModel.w)
        allLoss =
            logisticModel.loss
    in
    List.map
        (\index ->
            let
                x1 =
                    Maybe.withDefault [] <| nth 0 logisticModel.x
                x2 =
                    Maybe.withDefault [] <| nth 1 logisticModel.x
                activations =
                    [ [ Maybe.withDefault 0 <| nth index x1
                        , Maybe.withDefault 0 <| nth index x2
                        ]
                    , [ Maybe.withDefault 0 <| nth index allActivations ]
                    ]
                weights =
                    [[[ Maybe.withDefault 0 <| nth index allWeights ]]]
                loss =
                    case nth index allLoss of
                        Nothing ->
                            []
                        Just singleLoss ->
                            [ singleLoss ]
            in
            generateNet layers demoModel.height demoModel.width activations weights
        )
        (List.range 0 epochNumber)


getContentName : Int -> String
getContentName index =
    let
        lastIndex =
            List.length contentNames - 1
    in
    if index > lastIndex then
        Maybe.withDefault firstContentName <| nth lastIndex contentNames
    else
        Maybe.withDefault firstContentName <| nth index contentNames


getContentIndex : String -> Int
getContentIndex name =
    Maybe.withDefault 0 <| List.Extra.elemIndex name contentNames


forwardOneLayer : Model -> (Model, Cmd Msg)
forwardOneLayer model =
    let
        currentLayerIndex =
            Tuple.first model.currentPosition

        currentIndex =
            Tuple.second model.currentPosition

        numberOfLayers =
            List.length model.layers

        currentLayerLength =
            Maybe.withDefault 0 (nth currentLayerIndex model.layers)
    in
    if currentIndex < currentLayerLength - 1 then
        forwardOneStep model

    else if currentIndex == currentLayerLength - 1 then
        if currentLayerIndex == numberOfLayers - 1 then
            forwardOneStep model
        else -- todo
            forwardOneLayer (Tuple.first <| forwardOneStep model)
    else
        ( model
        , Cmd.none
        )


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


forwardOneStep : Model -> (Model, Cmd Msg)
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

        currentIndex =
            Tuple.second model.currentPosition
        
        nextNet =
            if currentIndex >= layerLength - 1 then
                updateActivations (currentLayerIndex + 1) 0 model.net model.nextNet
            else
                updateActivations currentLayerIndex (currentIndex + 1) model.net model.nextNet
    in
    if currentIndex >= layerLength - 1 then
        if currentLayerIndex >= numberOfLayers - 1 then
            let
                beforeEndOfEpoch =
                    model.netIndex < List.length model.nets
            in
            ({ model
                | currentPosition = ( numberOfLayers - 1, layerLength - 1 )
                , currentDirection = Backward
                , nextNet =
                    if beforeEndOfEpoch then
                        Maybe.withDefault emptyNet <| nth (model.netIndex + 1) model.nets
                    else
                        emptyNet
                , netIndex =
                    if beforeEndOfEpoch then
                        model.netIndex + 1
                    else
                        model.netIndex
            }
            , if beforeEndOfEpoch then
                Cmd.none
            else
                getEpoch model.demoId
            )

        else
            ({ model
                | currentPosition = ( currentLayerIndex + 1, 0 )
                , net = nextNet
            }
            , Cmd.none
            )
    else
        ({ model
            | currentPosition = ( currentLayerIndex, currentIndex + 1 )
            , net = nextNet
        }
        , Cmd.none
        )


backwardOneStep : Model -> Model
backwardOneStep model =
    let
        currentLayerIndex =
            Tuple.first model.currentPosition

        currentIndex =
            Tuple.second model.currentPosition

        nextLayerLength =
            Maybe.withDefault 0 (nth (currentLayerIndex - 1) model.layers)

        nextNet =
            updateWeights currentLayerIndex currentIndex model.net model.nextNet
    in
    if currentIndex <= 0 then
        if currentLayerIndex <= 1 then
            { model
                | currentPosition = ( 0, 0 )
                , currentDirection = Forward
                , net = clearActivationsExceptFirst model.nextNet
            }

        else
            { model
                | currentPosition = ( currentLayerIndex - 1, nextLayerLength - 1 )
                , net = nextNet
            }

    else
        { model
            | currentPosition = ( currentLayerIndex, currentIndex - 1 )
            , net = nextNet
        }


initDemo =
    Http.post
        { url = serverRoot ++ "api/model/new"
        , body = Http.emptyBody
        , expect = Http.expectString GetDemoId
        }


getEpoch : String -> Cmd Msg
getEpoch demoId =
    Http.post
        { url = serverRoot ++ "api/model/iter"
        , body = Http.jsonBody <| Encode.object
            [ ("session_id", Encode.string demoId)
            , ("epoch_num", Encode.int 1)
            , ("learning_rate", Encode.float 0.01)
            ]
        , expect = Http.expectJson GetNextEpoch epochDecoder
        }


epochDecoder : Decode.Decoder LogisticRegressionModel
epochDecoder =
    Decode.map5 LogisticRegressionModel
        (Decode.field "X" <| Decode.list (Decode.list Decode.float))
        (Decode.field "Y" <| Decode.list (Decode.list Decode.float))
        (Decode.field "W" <| Decode.list (Decode.list (Decode.list Decode.float)))
        (Decode.field "A" <| Decode.list (Decode.list (Decode.list Decode.float)))
        (Decode.field "loss" <| Decode.list Decode.float)


emptyNode : Node
emptyNode =
    { x = 0
    , y = 0
    , pos = ( 0, 0 )
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
        displayLayerNodes _ currLayer =
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
                    nodeLayerIndex
                        < currentLayerIndex
                        || (nodeLayerIndex == currentLayerIndex && nodeIndex <= currentIndex)

                Backward ->
                    nodeLayerIndex
                        > currentLayerIndex
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
                , lineWidth (model.nodeRadius * 0.1)
                ]
                [ circle
                    ( node.x, node.y )
                    model.nodeRadius
                ]
            , text
                [ font
                    { size = round (model.nodeRadius * 0.8)
                    , family = "sans-serif"
                    }
                , align Center
                , baseLine Middle
                , fill (highContrast node.activation)
                ]
                ( node.x, node.y )
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
                    (if isVisitedNode start model.currentPosition then
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
                [ path ( start.x, start.y )
                    [ lineTo ( end.x, end.y )
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
                        [ rect ( node.x + width, node.y - height / 2 ) width height
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
                        ( node.x + width * 3 / 2, node.y )
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
            flatten2D
                (List.map2 func
                    (case List.Extra.init model.net of
                        Just initial ->
                            [] :: initial

                        Nothing ->
                            [ [] ]
                    )
                    model.net
                )

        clearBackground : List Renderable
        clearBackground =
            [ shapes
                [ fill Color.white ]
                [ rect ( 0, 0 ) (toFloat model.width) (toFloat model.height)
                ]
            ]
    in
    Canvas.toHtml ( model.width, model.height )
        []
        (clearBackground
            ++ displayLayers displayLayerEdges
            ++ displayLayers displayLayerNodes
        )



-- ++ flatten2D displayLosses)


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
                [ E.paddingEach { edges | bottom = 10 }
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
                    if model.currentPosition == ( 0, 0 ) then
                        E.rgba255 51 255 51 0.8

                    else
                        E.rgba255 51 255 51 0.3

                Backward ->
                    if model.currentPosition == ( lastLayerIndex, lastIndex ) then
                        E.rgba255 255 51 0 0.8

                    else
                        E.rgba255 255 51 0 0.3
    in
    E.el
        [ E.padding 10
        , Background.color background
        ]
        (E.text ("In " ++ direction ++ " propagation"))


contentNavigation : Model -> E.Element Msg
contentNavigation model =
    let
        prevUrl =
            getContentName (model.contentIndex - 1)
        nextUrl =
            getContentName (model.contentIndex + 1)
    in
    E.row
        [ E.paddingXY 20 10
        , E.width E.fill
        ]
        [ E.link
            [ E.alignLeft
            , E.mouseOver [ Font.color grey ]
            , E.pointer
            ] <|
            { url = prevUrl
            , label =
                E.html (FeatherIcons.arrowLeft |> FeatherIcons.toHtml [])
            }
        , E.link
            [ E.alignRight
            , E.mouseOver [ Font.color grey ]
            , E.pointer
            ] <|
            { url = nextUrl
            , label = E.html (FeatherIcons.arrowRight |> FeatherIcons.toHtml [])
            }
        ]


getCurrentNode : Model -> Node
getCurrentNode model =
    let
        layerIndex =
            Tuple.first model.currentPosition

        index =
            Tuple.second model.currentPosition
    in
    Maybe.withDefault emptyNode <|
        nth
            index
        <|
            Maybe.withDefault [] (nth layerIndex model.nextNet)


calculationDisplay : Model -> E.Element Msg
calculationDisplay model =
    let
        currNode =
            getCurrentNode model

        currAcitvation =
            currNode.activation

        currWeights =
            currNode.weights

        currLayerIndex =
            Tuple.first model.currentPosition

        prevLayer =
            nth (currLayerIndex - 1) model.net
    in
    case model.currentDirection of
        Forward ->
            case prevLayer of
                Nothing ->
                    E.none

                Just layer ->
                    let
                        terms =
                            List.map2 Tuple.pair currWeights (List.map .activation layer)
                    in
                    E.el
                        [ E.centerX
                        ]
                        (case terms of
                            [] ->
                                E.text ""

                            _ ->
                                E.wrappedRow
                                    [ E.spacingXY 0 10
                                    , Background.color (adjustAlpha bgColor 0.8)
                                    , E.paddingEach { edges | bottom = 15 }
                                    ]
                                    (E.text (model.activationFunction ++ "(")
                                        :: List.foldl
                                            (\( weight, activation ) list ->
                                                list
                                                    ++ [ case List.length list of
                                                            0 ->
                                                                E.none

                                                            _ ->
                                                                E.text " + "
                                                       , highlightWithBorder activation
                                                       , E.text " * "
                                                       , highlight weight
                                                       ]
                                            )
                                            []
                                            terms
                                        ++ [ E.text ") = "
                                           , highlightWithBorder currAcitvation
                                           ]
                                    )
                        )

        Backward ->
            E.none


highlight : Float -> E.Element Msg
highlight n =
    highlightHelper n False


highlightWithBorder : Float -> E.Element Msg
highlightWithBorder n =
    highlightHelper n True


highlightHelper : Float -> Bool -> E.Element Msg
highlightHelper n hasBorder =
    E.el
        [ Background.color <| toElmUIColor <| greenScale n
        , Font.color <| toElmUIColor <| highContrast n
        , E.padding 3
        , Border.rounded 10
        , Border.width 3
        , if hasBorder then
            Border.color <| toElmUIColor <| Color.yellow

          else
            Border.color <| E.rgba 0 0 0 0
        ]
        (E.text (Round.round 2 n))


view : Model -> Html Msg
view model =
    E.layout
        []
    <|
        E.wrappedRow
            [ E.width E.fill
            , E.height E.fill
            ]
            [ viewTutorialText model
            , viewTutorialDemo model
            ]


viewTutorialDemo : Model -> E.Element Msg
viewTutorialDemo model =
    case nth model.contentIndex contentDemos of
        Nothing ->
            E.none
        Just hasDemo ->
            if hasDemo then
                E.column
                [ E.width (E.fillPortion 5)
                , E.spacing 10
                , E.inFront (calculationDisplay model)
                ]
                (centerAll <|
                    case model.serverError of
                        Nothing ->
                            [ E.html (neuralNet model)
                            , directionTracker model
                            , controls model
                            ]
                        Just error ->
                            [ E.text error
                            ]
                )
            else
                E.none


viewTutorialText : Model -> E.Element Msg
viewTutorialText model =
    E.column
        [ E.width (E.fillPortion 3 |>
            E.minimum 360
        )
        , E.paddingXY 20 0
        , E.htmlAttribute (Html.Attributes.style "max-width" "70vw")
        , E.htmlAttribute (Html.Attributes.style "margin" "auto")
        , E.htmlAttribute (Html.Attributes.style "height" "calc(100vh - 20px)")
        ]
        [ contentNavigation model
        , E.html <| Html.div [ Html.Attributes.class "content" ] []
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


centerAll : List (E.Element msg) -> List (E.Element msg)
centerAll elements =
    List.map
        center
        elements


center : E.Element msg -> E.Element msg
center element =
    E.el
        [ E.htmlAttribute (Html.Attributes.style "marginLeft" "auto")
        , E.htmlAttribute (Html.Attributes.style "marginRight" "auto")
        ]
        element


lightGrey : E.Color
lightGrey =
    E.rgb 0.8 0.8 0.8


grey : E.Color
grey =
    E.rgb 0.6 0.6 0.6


darkGrey : E.Color
darkGrey =
    E.rgb 0.4 0.4 0.4


bgColor : E.Color
bgColor =
    E.rgb 1 1 1


adjustAlpha : E.Color -> Float -> E.Color
adjustAlpha oldColor newAlpha =
    let
        oldColorChannels =
            E.toRgb oldColor
    in
    E.fromRgb { oldColorChannels | alpha = newAlpha }


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
        Color.hsl 0 0.9 lightness

    else
        Color.hsl 0.3 0.9 lightness


toElmUIColor : Color.Color -> E.Color
toElmUIColor color =
    let
        { red, green, blue, alpha } =
            Color.toRgba color
    in
    E.rgba red green blue alpha


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
                value + 0.1

            else
                value
    in
    returnValue


highContrast : Float -> Color.Color
highContrast scale =
    let
        value =
            if abs scale > 1 then
                1

            else if 1 - scale < 0.5 then
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
    if n < 0 then
        Nothing

    else
        List.head (List.drop n xs)



-- source: https://github.com/elm/core/issues/968


tanh : Float -> Float
tanh x =
    (e ^ x - e ^ -x) / (e ^ x + e ^ -x)


repeat : Int -> (a -> a) -> a -> a
repeat steps func arg =
    if steps > 0 then
        repeat (steps - 1) func (func arg)

    else
        arg
