module Page.NotFound exposing (main, Model, Msg, view)

import Browser
import Html exposing (Html)
import Html.Attributes
import Element as E
import Element.Font as Font
import Element.Background as Background

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Model =
  {}

type Msg
  = NoOp


theme =
  { yellow = E.rgb255 247 203 55
  , dark = E.rgb255 50 29 29
  }


subscriptions : Model -> Sub msg
subscriptions _ =
  Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
  ( {}
  , Cmd.none
  )

view : Model -> Html Msg
view model =
  E.layout
    [ Font.family
        [ Font.external
          { name = "Nunito"
          , url = "https://fonts.googleapis.com/css?family=Nunito"
          }
        , Font.sansSerif
        ]
    , Font.color <| theme.yellow
    , Font.size 50
    , Font.glow (E.rgba255 255 218 94 0.8) 3
    , Background.color <| theme.dark
    , E.width E.fill
    , E.height E.fill
    , E.centerY
    , Font.center
    , E.padding 10
    ]
    ( E.el
        [ E.htmlAttribute <| Html.Attributes.style "margin-top" "45vh"
        , E.width E.fill
        ]
        (E.text "This page does not exist!")
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      ( model
      , Cmd.none
      )