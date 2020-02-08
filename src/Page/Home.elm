module Page.Home exposing (main, Model, Msg, init, update, view)

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
  { courses : List String
  }

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
  ( { courses =
    [ "Intro to Machine Learning"
    , "Intro to Deep Learning"
    , "Logistic Regression Model"
    ]
  }
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
    , Font.glow (E.rgba255 255 218 94 0.8) 3
    , Background.color <| theme.dark
    , E.padding 10
    ]
    ( E.column
      [ E.width E.fill ]
      [ viewHeader
      , viewBody model
      ]
    )

viewHeader : E.Element Msg
viewHeader =
  E.row
    [ E.width E.fill ]
    [ E.el
      [ E.alignLeft ] 
      ( E.text "AIWaffle"
      )
    , E.el
      [ E.alignRight ]
      ( E.text "Log In"
      )
    ]


viewBody : Model -> E.Element Msg
viewBody model =
  E.column
    [ E.width E.fill
    , E.spacing 20
    , E.padding 30
    ]
    [ E.el
      [ E.centerX
      , Font.bold
      ]
      (E.text "Develop Your AI Superpower")
    , E.paragraph
      [ E.centerX
      , Font.center
      , E.width (E.fill |> E.maximum 600)
      ]
      [ E.text "learn AI Interactively from scratch for free. \nUnderstand the workings behind the AI superpower, \nand start using them yourself!"
      ]
    , E.wrappedRow
      [ E.centerX
      , E.padding 20
      , E.spacing 40
      ]
      ( List.map
        viewCourseCard
        model.courses
      )
    ]


viewCourseCard title =
  E.el
    [ Background.image "/assets/waffle.png"
    , E.width <| E.px 200
    , E.height <| E.px 200
    , E.htmlAttribute <| Html.Attributes.style "margin" "auto"
    ]
    ( E.paragraph
      [ Font.center
      , Font.color theme.dark
      , Font.bold
      , E.centerY
      ]
      [ E.link []
        { url = "/tutorial/" ++ title
        , label = E.text title
        }
      ]
      )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      ( model
      , Cmd.none
      )