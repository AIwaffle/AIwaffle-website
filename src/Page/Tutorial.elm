port module Page.Tutorial exposing (Model, Msg, init, subscriptions, update, view, getContentName)

import Browser
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
import FeatherIcons
import Http
import Demo.LogisticRegression as Demo
import VegaLite as Vega

port renderContent : (String -> Cmd msg)
port elmToJs : Vega.Spec -> Cmd msg
port scrollToTop : () -> Cmd msg

type alias Model =
    { contentIndex : Int
    , demo : Demo.Model
    , showMenu : Bool
    }

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


theme =
  { yellow = E.rgb255 247 203 55
  , darkYellow = E.rgb255 235 182 0
  , grey = E.rgb255 170 170 170
  , dark = E.rgb255 50 29 29
  }


init : String -> ( Model, Cmd Msg )
init contentName =
    let
        (demo, initDemoMsg) =
            Demo.init
    in
    ({ contentIndex =
        getContentIndex contentName 
    , demo =
        demo
    , showMenu =
        True
    }
    , Cmd.batch
        [ renderContent contentName
        , Cmd.map DemoMsg initDemoMsg
        , scrollToTop ()
        ]
    )


type Msg
    = GetContentFromName String
    | DemoMsg Demo.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
        DemoMsg demoMsg ->
            let
                (newDemo, newDemoMsg) =
                    Demo.update demoMsg model.demo
            in
            ( { model |
                demo =
                    newDemo
            }
            , Cmd.batch
                [ Cmd.map DemoMsg newDemoMsg
                , elmToJs newDemo.demoSpecs
                ]
            )


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


view : Model -> Html Msg
view model =
    E.layout
        []
        <|
        E.wrappedRow
            [ E.width E.fill
            , E.height E.fill
            ]
            [ viewTutorialMenu model
            , viewTutorialText model
            , viewTutorialDemo model
            , viewNextButton model
            ]


viewTutorialMenu : Model -> E.Element Msg
viewTutorialMenu model =
    if model.showMenu then
        E.column
            [ E.htmlAttribute <| Html.Attributes.style "width" "300px"
            , E.htmlAttribute <| Html.Attributes.style "height" "100vh"
            , E.htmlAttribute <| Html.Attributes.style "position" "fixed"
            , E.htmlAttribute <| Html.Attributes.id "tutorial-menu"
            , E.htmlAttribute <| Html.Attributes.style "z-index" "9999"
            , E.height <| E.fill
            , E.spacing 30
            , E.padding 20
            , Background.color theme.dark
            , Font.color theme.yellow
            ]
            ( E.link
                [ E.width E.fill
                , Border.width 2
                , E.padding 10
                ]
                { url = "/"
                , label = E.text "Home"
                }
            :: List.indexedMap
                (\contentIndex contentName ->
                    E.link []
                        { url = contentName
                        , label =
                            E.el
                            ( if contentIndex == model.contentIndex then
                                [ Font.bold
                                , Font.color theme.yellow
                                ]
                            else
                                [ Font.regular
                                , Font.color theme.grey
                                ]
                            )
                            (E.text contentName)
                        }
                )
                contentNames
            )
    else
        E.none


viewTutorialDemo : Model -> E.Element Msg
viewTutorialDemo model =
    case nth model.contentIndex contentDemos of
        Nothing ->
            E.none
        Just hasDemo ->
            if hasDemo then
                E.column
                [ E.width (E.fillPortion 6)
                , E.spacing 10
                , E.paddingEach
                    { top = 0
                    , left = 0
                    , bottom = 70
                    , right = 0
                    }
                ]
                [ center <|
                    E.map DemoMsg <| Demo.view model.demo
                ]
            else
                E.none


viewTutorialText : Model -> E.Element Msg
viewTutorialText model =
    E.column
        [ E.width (E.fillPortion 5 |>
            E.minimum 360
        )
        , E.paddingXY 20 0
        , E.htmlAttribute (Html.Attributes.style "max-width" "70vw")
        , E.htmlAttribute (Html.Attributes.style "margin" "auto")
        , E.htmlAttribute (Html.Attributes.style "margin-top" "20px")
        ]
        [ E.html <| Html.div
            [ Html.Attributes.class
                (case nth model.contentIndex contentDemos of
                    Nothing ->
                        "content"
                    Just hasDemo ->
                        if hasDemo then
                            "content content-scroll"
                        else
                            "content"
                )
            ]
            []
        ]


viewNextButton : Model -> E.Element Msg
viewNextButton model =
    E.link
        [ E.htmlAttribute <| Html.Attributes.style "position" "fixed"
        , E.htmlAttribute <| Html.Attributes.style "bottom" "20px"
        , E.htmlAttribute <| Html.Attributes.style "right" "20px"
        ]
        { url = getContentName (model.contentIndex + 1)
        , label =
            Input.button
                [ Background.color theme.yellow
                , E.padding 10
                , E.mouseOver
                [ Background.color theme.darkYellow
                ]
                ]
                { onPress = Nothing
                , label = E.text "Next"
                }
        }

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


nth : Int -> List a -> Maybe a
nth n xs =
    if n < 0 then
        Nothing

    else
        List.head (List.drop n xs)
