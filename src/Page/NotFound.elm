module Page.NotFound exposing (Model, Msg, main, view)

import Browser
import Element as E
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes
import Style


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
            [ Font.typeface "Nunito"
            , Font.sansSerif
            ]
        , Font.color <| Style.color.yellow
        , Font.size 50
        , Font.glow (E.rgba255 255 218 94 0.8) 3
        , Background.color <| Style.color.dark
        , E.width E.fill
        , E.height E.fill
        , E.centerY
        , Font.center
        , E.padding 10
        ]
        (E.el
            [ E.htmlAttribute <| Html.Attributes.style "margin-top" "45vh"
            , E.width E.fill
            ]
            (E.paragraph [] [ E.text "This page does not exist!" ])
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )
