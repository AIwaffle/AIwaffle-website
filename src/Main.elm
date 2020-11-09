port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html
import Page.About as About
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Tutorial as Tutorial
import SharedState exposing (SharedState, UpdateSharedState)
import Tuple3
import Url
import Url.Parser as Parser exposing ((</>), Parser)


port getUsername : (() -> msg) -> Sub msg
port setUsername : String -> Cmd msg


-- MAIN


main : Program (Maybe String) Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , page : Page
    , sharedState : SharedState
    }


type Page
    = NotFound
    | Home Home.Model
    | Tutorial Tutorial.Model
    | About About.Model


init : Maybe String -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeUsername url key =
    let
        initialSharedState =
            case maybeUsername of
                Just username ->
                    { username = username
                    , password = ""
                    , loggedIn = True
                    }
                
                Nothing ->
                    { username = ""
                    , password = ""
                    , loggedIn = False
                    }
    in
    route url
        { key = key
        , page = Home <| Tuple3.first <| Home.init ()
        , sharedState = initialSharedState
        }



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NotFoundMsg NotFound.Msg
    | HomeMsg Home.Msg
    | TutorialMsg Tutorial.Msg
    | AboutMsg About.Msg
    | SaveUsername


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            route url model

        HomeMsg msg ->
            case model.page of
                Home home ->
                    stepHome model (Home.update model.sharedState msg home)

                _ ->
                    ( model, Cmd.none )

        TutorialMsg msg ->
            case model.page of
                Tutorial tutorial ->
                    stepTutorial model (Tutorial.update msg tutorial)

                _ ->
                    ( model, Cmd.none )

        NotFoundMsg _ ->
            ( model, Cmd.none )

        AboutMsg _ ->
            ( model, Cmd.none )

        SaveUsername ->
            ( model, setUsername model.sharedState.username )



route : Url.Url -> Model -> ( Model, Cmd Msg )
route url model =
    let
        parser =
            Parser.oneOf
                [ Parser.map
                    (stepHome model (Home.init ()))
                    (Parser.s "home")
                , Parser.map
                    (\tutorialName ->
                        let
                            name =
                                Maybe.withDefault tutorialName <| Url.percentDecode tutorialName
                        in
                        stepTutorial model (Tutorial.init (model.sharedState, name))
                    )
                    (Parser.s "tutorial" </> tutorialName_)
                , Parser.map
                    (stepAbout model (About.init ()))
                    (Parser.s "about")
                , Parser.map
                    (stepHome model (Home.init ()))
                    Parser.top
                ]
    in
    case Parser.parse parser url of
        Just answer ->
            answer

        Nothing ->
            ( { model | page = NotFound }
            , Cmd.none
            )


tutorialName_ : Parser (String -> a) a
tutorialName_ =
    Parser.custom "TUTORIAL" Just


stepHome : Model -> ( Home.Model, Cmd Home.Msg, UpdateSharedState ) -> ( Model, Cmd Msg )
stepHome model ( home, cmds, updateSharedState ) =
    ( { model
        | page = Home home
        , sharedState =
            SharedState.update model.sharedState updateSharedState
      }
    , Cmd.map HomeMsg cmds
    )


stepTutorial : Model -> ( Tutorial.Model, Cmd Tutorial.Msg ) -> ( Model, Cmd Msg )
stepTutorial model ( tutorial, cmds ) =
    ( { model
        | page = Tutorial tutorial
      }
    , Cmd.map TutorialMsg cmds
    )


stepAbout : Model -> ( About.Model, Cmd About.Msg ) -> ( Model, Cmd Msg )
stepAbout model ( about, cmds ) =
    ( { model | page = About about }
    , Cmd.map AboutMsg cmds
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    getUsername (\_ -> SaveUsername)



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.page of
        NotFound ->
            { title = "AIwaffle"
            , body =
                [ Html.map NotFoundMsg <| NotFound.view {}
                ]
            }

        Home home ->
            { title = "AIwaffle"
            , body =
                [ Html.map HomeMsg <| Home.view model.sharedState home
                ]
            }

        Tutorial tutorial ->
            { title = Tutorial.getContentName tutorial.contentIndex
            , body =
                [ Html.map TutorialMsg <| Tutorial.view tutorial
                ]
            }

        About about ->
            { title = "About AIwaffle"
            , body =
                [ Html.map AboutMsg <| About.view about
                ]
            }
