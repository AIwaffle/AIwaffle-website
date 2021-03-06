port module Page.Tutorial exposing (Model, Msg, getContentId, getContentName, init, subscriptions, update, view)

import Browser
import Browser.Dom
import Browser.Events
import Color
import Constants exposing (courseDemos, courseIds, courseNames, discussionIds, forumRoot, markdownCourseIds)
import Demo.LogisticRegression as Demo
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Html exposing (Html)
import Html.Attributes
import Http
import List
import List.Extra
import SharedState exposing (SharedState)
import Style
import Task
import VegaLite as Vega


port renderContent : String -> Cmd msg


port elmToJs : Vega.Spec -> Cmd msg


port scrollToTop : () -> Cmd msg


type alias Model =
    { contentIndex : Int
    , demo : Demo.Model
    , showMenu : Bool
    , sharedState : SharedState
    , windowWidth : Float
    }


firstContentName : String
firstContentName =
    Maybe.withDefault "" <| List.head courseNames


init : ( SharedState, String ) -> ( Model, Cmd Msg )
init ( sharedState, courseId ) =
    let
        ( demo, initDemoMsg ) =
            Demo.init
    in
    ( { contentIndex =
            getContentIndex courseId
      , demo =
            demo
      , showMenu =
            True
      , sharedState =
            sharedState
      , windowWidth =
            400
      }
    , Cmd.batch
        [ renderContent courseId
        , Cmd.map DemoMsg initDemoMsg
        , scrollToTop ()
        , Task.perform (\{viewport} -> GetWindowWidth viewport.width) Browser.Dom.getViewport
        ]
    )


type Msg
    = GetContentFromName String
    | DemoMsg Demo.Msg
    | GetWindowWidth Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetContentFromName name ->
            let
                index =
                    getContentIndex name
            in
            ( { model
                | contentIndex = index
              }
            , renderContent name
            )

        DemoMsg demoMsg ->
            let
                ( newDemo, newDemoMsg ) =
                    Demo.update demoMsg model.demo
            in
            ( { model
                | demo =
                    newDemo
              }
            , Cmd.batch
                [ Cmd.map DemoMsg newDemoMsg
                , elmToJs newDemo.demoSpecs
                ]
            )

        GetWindowWidth width ->
            ( { model
                | windowWidth =
                    width
                , showMenu =
                    width >= 1000
              }
            , Cmd.none
            )


getContentId : Int -> String
getContentId index =
    let
        lastIndex =
            List.length courseIds - 1
    in
    if index > lastIndex then
        Maybe.withDefault firstContentName <| nth lastIndex courseIds

    else
        Maybe.withDefault firstContentName <| nth index courseIds


getContentName : Int -> String
getContentName index =
    let
        lastIndex =
            List.length courseNames - 1
    in
    if index > lastIndex then
        Maybe.withDefault firstContentName <| nth lastIndex courseNames

    else
        Maybe.withDefault firstContentName <| nth index courseNames


getContentIndex : String -> Int
getContentIndex courseId =
    Maybe.withDefault 0 <| List.Extra.elemIndex courseId courseIds


view : Model -> Html Msg
view model =
    E.layout
        [ E.inFront <| viewTutorialMenu model ]
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
            [ E.width <| E.px 300
            , E.height <| E.fill
            , E.spacing 30
            , E.padding 20
            , Background.color Style.color.dark
            , Font.color Style.color.yellow
            ]
            (E.link
                [ E.width E.fill
                , Border.width 2
                , E.padding 10
                ]
                { url = "/"
                , label =
                    E.row []
                        [ E.image
                            [ E.htmlAttribute <| Html.Attributes.class "inline-logo"
                            ]
                            { src = "/assets/logo.svg"
                            , description = "AIwaffle Logo"
                            }
                        , E.text "Home"
                        ]
                }
                :: List.indexedMap
                    (\contentIndex ( courseId, courseName ) ->
                        E.link []
                            { url = courseId
                            , label =
                                E.el
                                    (if contentIndex == model.contentIndex then
                                        [ Font.bold
                                        , Font.color Style.color.yellow
                                        ]

                                     else
                                        [ Font.regular
                                        , Font.color Style.color.grey
                                        ]
                                    )
                                    (E.paragraph [] [ E.text courseName ])
                            }
                    )
                    (List.map2 Tuple.pair courseIds courseNames)
            )

    else
        -- need a empty placeholder element instead of E.none
        E.el [] E.none


viewTutorialDemo : Model -> E.Element Msg
viewTutorialDemo model =
    case nth model.contentIndex courseDemos of
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
                        E.map DemoMsg <|
                            Demo.view model.demo
                    ]

            else
                E.none


viewTutorialText : Model -> E.Element Msg
viewTutorialText model =
    let
        contentId =
            getContentId model.contentIndex
    in
    E.column
        [ E.width
            (E.fillPortion 5
                |> E.minimum 360
            )
        , E.paddingXY 20 0
        ]
        [ E.el
            [ E.inFront <|
                if model.sharedState.loggedIn && not (List.member contentId markdownCourseIds) then
                    E.newTabLink
                        [ Font.color Style.color.yellow
                        , E.below <|
                            E.el
                                [ E.centerX
                                , Font.color Style.color.dark
                                ]
                                (E.text "Run")
                        ]
                        { url =
                            "/jhub/user/" ++ model.sharedState.username ++ "/notebooks/Courses/" ++ contentId ++ ".ipynb"
                        , label =
                            E.html
                                (FeatherIcons.playCircle
                                    |> FeatherIcons.withSize 80
                                    |> FeatherIcons.withStrokeWidth 4.2
                                    |> FeatherIcons.toHtml []
                                )
                        }

                else
                    E.none
            , E.width E.fill
            ]
          <|
            E.html <|
                Html.div
                    [ Html.Attributes.class
                        (case nth model.contentIndex courseDemos of
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
        , case nth model.contentIndex discussionIds of
            Just id ->
                E.html <|
                    Html.iframe
                        [ Html.Attributes.class "discussion"
                        , Html.Attributes.src <| forumRoot ++ id
                        , Html.Attributes.style "width" "100%"
                        , Html.Attributes.style "min-height" "500px"
                        , Html.Attributes.style "margin-bottom" "20px"
                        , Html.Attributes.style "border" "none"
                        ]
                        []

            Nothing ->
                E.none
        ]


viewNextButton : Model -> E.Element Msg
viewNextButton model =
    E.link
        [ E.htmlAttribute <| Html.Attributes.style "position" "fixed"
        , E.htmlAttribute <| Html.Attributes.style "bottom" "20px"
        , E.htmlAttribute <| Html.Attributes.style "right" "20px"
        ]
        { url = getContentId (model.contentIndex + 1)
        , label =
            Input.button
                [ Background.color Style.color.yellow
                , E.padding 10
                , E.mouseOver
                    [ Background.color Style.color.darkYellow
                    ]
                ]
                { onPress = Nothing
                , label = E.text "Next"
                }
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\w _ -> GetWindowWidth <| toFloat w)


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
