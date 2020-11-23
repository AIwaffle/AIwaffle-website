port module Page.About exposing (Model, Msg, init, main, view, subscriptions)

import Browser
import Element as E
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes
import Style


port renderGithubCards : () -> Cmd msg


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
    , renderGithubCards ()
    )


view : Model -> Html Msg
view model =
    E.layout
        [ Font.family
            [ Font.typeface "Nunito"
            , Font.sansSerif
            ]
        , Font.color <| Style.color.yellow
        , Font.glow (E.rgba255 255 218 94 0.8) 3
        , Background.color <| Style.color.dark
        ]
    <|
        E.column
            [ E.centerX
            , E.spacing 20
            , E.padding 10
            , E.width (E.fill |> E.maximum 800)
            ]
            [ viewHeader
            , E.row
                (E.centerX
                    :: Style.font.title
                )
                [ E.image
                    [ E.htmlAttribute <| Html.Attributes.class "inline-logo"
                    ]
                    { src = "/assets/logo.svg"
                    , description = "AIwaffle Logo"
                    }
                , E.paragraph [] [ E.text "About AIwaffle" ]
                ]
            , E.paragraph [ E.centerX ]
                [ E.text "AIwaffle is an AI Learning Platform completely made by high school students.\n"
                , E.text "We aim at AI beginners and provide original courses, extensive materials, and periodic blogs on AI, OI, and more."
                ]
            , E.el Style.font.subtitle <|
                E.text "Repository"
            , E.paragraph [ E.centerX ]
                [ E.text "We are committed to open sourcing our contents to benefit AI learners around the world."
                ]
            , viewRepoGithubCard "AIwaffle"
            , E.el Style.font.subtitle <|
                E.text "Collaborators"
            , E.wrappedRow [ E.centerX ] <|
                List.map viewUserGithubCard [ "IDl0T", "AlienKevin", "jimmy-zx", "wlt233", "nichujie", "xuyinjiesh", "MistakableQwQ" ]
            , E.el Style.font.subtitle <|
                E.text "Blog"
            , E.paragraph [ E.centerX ]
                [ E.text "We share our ideas on AI, intersting AI projects, and coding competitions in our blog."
                ]
            , E.newTabLink Style.font.link
                { url = "https://aiwaffle.github.io/AIwaffle-blog/"
                , label = E.text "Check out our blog here."
                }
            ]


viewHeader : E.Element Msg
viewHeader =
    E.row
        [ E.width E.fill
        , E.padding 10
        , E.spacing 20
        ]
        [ E.link
            [ E.alignLeft
            , Font.underline
            ]
            { url = "/"
            , label = E.text "Home"
            }
        ]


viewRepoGithubCard : String -> E.Element Msg
viewRepoGithubCard repoName =
    viewGithubCard "data-repo" repoName


viewUserGithubCard : String -> E.Element Msg
viewUserGithubCard userName =
    viewGithubCard "data-user" userName


viewGithubCard : String -> String -> E.Element Msg
viewGithubCard attributeName attributeValue =
    E.html <|
        Html.div
            [ Html.Attributes.class "github-card"
            , Html.Attributes.attribute "data-user" attributeValue
            , Html.Attributes.attribute attributeName attributeValue
            , Html.Attributes.attribute "data-width" "300"
            , Html.Attributes.attribute "data-height" ""
            , Html.Attributes.attribute "data-theme" "default"
            , Html.Attributes.attribute "data-target" "blank"
            ]
            []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )
