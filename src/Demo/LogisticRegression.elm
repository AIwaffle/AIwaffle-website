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


type alias LogisticRegressionModel =
  { x : Floats2
  , y : Floats2
  , w : Floats3
  , a : Floats3
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
  , serverError : Maybe String
  }

emptyModel : Model
emptyModel =
  { demoId = ""
  , demoModel =
    emptyLogisticRegressionModel
  , serverError =
    Nothing
  }


type Msg
  = LoggedIn (Result Http.Error ())
  | GetDemoId (Result Http.Error String)
  | GetNextEpoch (Result Http.Error LogisticRegressionModel)


serverRoot : String
serverRoot =
  "http://106.15.39.117:8080/"


emptyLogisticRegressionModel =
  { x = []
  , y = []
  , w = []
  , a = []
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
            demoModel =
              logisticRegressionModel
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
  Decode.map5 LogisticRegressionModel
    (Decode.field "X" <| Decode.list (Decode.list Decode.float))
    (Decode.field "Y" <| Decode.list (Decode.list Decode.float))
    (Decode.field "W" <| Decode.list (Decode.list (Decode.list Decode.float)))
    (Decode.field "A" <| Decode.list (Decode.list (Decode.list Decode.float)))
    (Decode.field "loss" <| Decode.list Decode.float)

view : Model -> E.Element Msg
view model =
  E.text <| "logistic regression demo"
