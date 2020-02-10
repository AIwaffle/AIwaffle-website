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

port renderContent : (String -> Cmd msg)

type alias Model =
    { contentIndex : Int
    , demo : Demo.Model
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
        (demo, initDemoMsg) =
            Demo.init
    in
    ({ contentIndex =
        getContentIndex contentName 
    , demo =
        demo
    }
    , Cmd.batch
        [ renderContent contentName
        , Cmd.map DemoMsg initDemoMsg
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
            , Cmd.map DemoMsg newDemoMsg
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
            let
                _ = Debug.log "hasDemo" hasDemo
                _ = Debug.log "model.contentIndex" model.contentIndex
            in
            if hasDemo then
                E.column
                [ E.width (E.fillPortion 5)
                , E.spacing 10
                ]
                [ center <|
                    E.map DemoMsg <| Demo.view model.demo
                ]
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


nth : Int -> List a -> Maybe a
nth n xs =
    if n < 0 then
        Nothing

    else
        List.head (List.drop n xs)
