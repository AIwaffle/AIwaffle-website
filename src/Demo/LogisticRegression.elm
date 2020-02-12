module Demo.LogisticRegression exposing (Model, Msg, init, update, view)

import Color
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import List
import List.Extra
import Round
import Http
import Json.Encode as Encode
import Json.Decode as Decode
import VegaLite as Vega


type alias LogisticRegressionModel =
  { x : Floats2
  , y : Floats2
  , w : Floats3
  , loss : Floats1
  }


type alias Floats1 =
  List Float


type alias Floats2 =
  List (List Float)


type alias Floats3 =
  List (List (List Float))


type alias Model =
  { demoId : String
  , demoModel : LogisticRegressionModel
  , demoSpecs : Vega.Spec
  , serverError : Maybe String
  }

emptyModel : Model
emptyModel =
  { demoId = ""
  , demoModel =
    emptyLogisticRegressionModel
  , demoSpecs =
    emptySpec
  , serverError =
    Nothing
  }


type Msg
  = LoggedIn (Result Http.Error ())
  | GetDemoId (Result Http.Error String)
  | GetNextEpoch (Result Http.Error LogisticRegressionModel)
  | Run Int


theme =
  { yellow = E.rgb255 247 203 55
  , black = E.rgb255 0 0 0
  , darkYellow = E.rgb255 235 182 0
  }


serverRoot : String
serverRoot =
  "/"


emptyLogisticRegressionModel =
  { x = []
  , y = []
  , w = []
  , loss = []
  }

init : ( Model, Cmd Msg )
init  =
    ( emptyModel
    , logIn
    )


logIn : Cmd Msg
logIn =
  Http.post
    { url = serverRoot ++ "auth/login"
    , body = Http.jsonBody <| Encode.object
      [ ( "username", Encode.string "admin" )
      , ( "password", Encode.string "040506" )
      ]
    , expect = Http.expectWhatever LoggedIn
  } 


initDemo =
  Http.post
    { url = serverRoot ++ "api/model/new"
    , body = Http.emptyBody
    , expect = Http.expectString GetDemoId
    }


getEpoch : Int -> String -> Cmd Msg
getEpoch epochNumber demoId =
  Http.post
    { url = serverRoot ++ "api/model/iter"
    , body = Http.jsonBody <| Encode.object
      [ ("session_id", Encode.string demoId)
      , ("epoch_num", Encode.int epochNumber)
      , ("learning_rate", Encode.float 0.01)
      ]
    , expect = Http.expectJson GetNextEpoch epochDecoder
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LoggedIn result ->
      ( case result of
      Err reason ->
        { model |
        serverError =
          Just "Can't log in to server."
        }
      Ok _ ->
        model
      , initDemo
      )

    GetDemoId result ->
      case result of
      Ok id ->
        ({ model |
        demoId =
          id
        }
        , getEpoch 1 id
        )
      Err _ ->
        ({ model |
        serverError =
          Just "Can't get demo from server."
        }
        , Cmd.none
        )

    Run epochNumber ->
      ( model
      , getEpoch epochNumber model.demoId
      )

    GetNextEpoch result ->
      (case result of
        Ok newDemoModel ->
          let
            storedDemoModel =
              { newDemoModel |
                  loss =
                    model.demoModel.loss ++ newDemoModel.loss
              }
          in
          { model |
            demoModel =
              storedDemoModel
            , demoSpecs =
              demoSpecs
                { model |
                  demoModel =
                    storedDemoModel
                }
          }
        Err _ ->
          { model |
            serverError =
              Just "Can't get next epoch from server."
          }
      , Cmd.none
      )


epochDecoder : Decode.Decoder LogisticRegressionModel
epochDecoder =
  Decode.map4 LogisticRegressionModel
    (Decode.field "X" <| Decode.list (Decode.list Decode.float))
    (Decode.field "Y" <| Decode.list (Decode.list Decode.float))
    (Decode.field "W" <| Decode.list (Decode.list (Decode.list Decode.float)))
    (Decode.field "loss" <| Decode.list Decode.float)


view : Model -> E.Element Msg
view model =
  E.column
    [ E.width E.fill
    , E.height E.fill
    ]
    [ E.el
      [ E.htmlAttribute <| Html.Attributes.id "logisticRegressionDemoScatterPlot"
      , E.width <| E.px 500
      , E.height <| E.px 400
      , E.centerX
      , E.htmlAttribute <| Html.Attributes.style "max-width" "100vw"
      , E.htmlAttribute <| Html.Attributes.style "max-height" "70vw"
      ]
      (E.none)
    , E.el
      [ E.htmlAttribute <| Html.Attributes.id "logisticRegressionDemoLossPlot"
      , E.width <| E.px 500
      , E.height <| E.px 300
      , E.centerX
      , E.htmlAttribute <| Html.Attributes.style "max-width" "100vw"
      , E.htmlAttribute <| Html.Attributes.style "max-height" "30vw"]
      (E.none)
    , E.wrappedRow
      [ E.spacing 20
      , E.width E.fill
      ]
      [ button "Run 1 Epoch" (Run 1)
      , button "Run 10 Epoch" (Run 10)
      , button "Run 100 Epoch" (Run 100)
      , button "Run 1000 Epoch" (Run 1000)
      ]
    ]


button : String -> Msg -> E.Element Msg
button text msg =
  Input.button
    [ Background.color theme.yellow
    , E.padding 10
    , E.mouseOver
      [ Background.color theme.darkYellow
      ]
    ]
    { onPress = Just msg
    , label = E.text text
    }


demoSpecs : Model -> Vega.Spec
demoSpecs model =
  Vega.combineSpecs
    [ ( "logisticRegressionDemoScatterPlot",
      let
        data =
          Vega.dataFromRows [] []

        encoding =
          Vega.encoding
      in
      Vega.toVegaLite
        [ Vega.widthOfContainer
        , Vega.heightOfContainer
        , data
        , encoding []
        , Vega.layer
          [ lineSpec model
          , scatterPlotSpec model
          ]
        ]
    )
    , ( "logisticRegressionDemoLossPlot"
    , lossPlotSpec model
    )
    ]


scatterPlotSpec : Model -> Vega.Spec
scatterPlotSpec model =
  let
    points =
      Vega.dataFromColumns []
        << Vega.dataColumn "x" (Vega.nums <| Maybe.withDefault [] <| List.Extra.getAt 0 model.demoModel.x)
        << Vega.dataColumn "y" (Vega.nums <| Maybe.withDefault [] <| List.Extra.getAt 1 model.demoModel.x)
        << Vega.dataColumn "group" (Vega.strs <|
          (List.map
            (\num ->
              case round num of
                0 ->
                  "Group 1"
                1 ->
                  "Group 2"
                _ ->
                  "Group"
            )
            (Maybe.withDefault [] <| List.Extra.getAt 0 model.demoModel.y)
          )
        )

    encoding =
      Vega.encoding
        << Vega.position Vega.X [ Vega.pName "x", Vega.pQuant ]
        << Vega.position Vega.Y [ Vega.pName "y", Vega.pQuant ]
        << Vega.color [ Vega.mName "group", Vega.mNominal ]
  in
  Vega.toVegaLite
    [ points []
    , encoding []
    , Vega.circle []
    ]

lineSpec : Model -> Vega.Spec
lineSpec model =
  let
    w =
      Maybe.withDefault [] <| List.Extra.getAt 0 <|
        Maybe.withDefault [] <| List.Extra.getAt 0 <|
          model.demoModel.w
    w1 =
      Maybe.withDefault 0 <| List.Extra.getAt 1 <| w
    _ =
      Debug.log "w1" w1
    w2 =
      Maybe.withDefault 0 <| List.Extra.getAt 2 <| w
    _ =
      Debug.log "w2" w2
    b =
      Maybe.withDefault 0 <| List.Extra.getAt 0 <| w
    _ =
      Debug.log "b" b
    x1 =
      0
    y1 =
      (-b) / w2
    x2 =
      1
    y2 =
      (-b - x2 * w1) / w2
    points =
        Vega.dataFromColumns []
          << Vega.dataColumn "x" (Vega.nums [x1, x2])
          << Vega.dataColumn "y" (Vega.nums [y1, y2])
    encoding =
      Vega.encoding
        << Vega.position Vega.X [ Vega.pName "x", Vega.pQuant ]
        << Vega.position Vega.Y [ Vega.pName "y", Vega.pQuant ]
  in
  Vega.toVegaLite
      [ points []
      , Vega.line
        [ Vega.maColor "#734FD8"
        ]
      , encoding []
      ]


lossPlotSpec : Model -> Vega.Spec
lossPlotSpec model =
  let
    _ = Debug.log "lossPlot model.demoModel.loss" <| model.demoModel.loss
    points =
      Vega.dataFromColumns []
        << Vega.dataColumn "epoch"
          (Vega.nums <| List.map toFloat <|
            List.range 0 (List.length model.demoModel.loss - 1)
          )
        << Vega.dataColumn "loss" (Vega.nums model.demoModel.loss)
    encoding =
      Vega.encoding
        << Vega.position Vega.X [ Vega.pName "epoch", Vega.pQuant ]
        << Vega.position Vega.Y [ Vega.pName "loss", Vega.pQuant ]
  in
  Vega.toVegaLite
      [ Vega.widthOfContainer
      , Vega.heightOfContainer
      , points []
      , Vega.line
        [ Vega.maColor "#734FD8"
        ]
      , encoding []
      ]


emptySpec : Vega.Spec
emptySpec =
  let
    data =
      Vega.dataFromRows [] []

    encoding =
      Vega.encoding
  in
  Vega.toVegaLite [ data, encoding [], Vega.circle [] ]