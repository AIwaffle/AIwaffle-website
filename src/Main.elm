module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html
import Page.Home as Home
import Page.NotFound as NotFound
import Page.Tutorial as Tutorial
import Url
import Url.Parser as Parser exposing ((</>), Parser)



-- MAIN


main : Program () Model Msg
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
  }


type Page
  = NotFound
  | Home Home.Model
  | Tutorial Tutorial.Model


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  route url { key = key, page = NotFound }



-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | NotFoundMsg NotFound.Msg
  | HomeMsg Home.Msg
  | TutorialMsg Tutorial.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
  case message of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          route url model |> reloadIfNeeded url model

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      route url model

    HomeMsg msg ->
      case model.page of
        Home home ->
          stepHome model (Home.update msg home)

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



-- ROUTING


type Route
  = HomeRoute
  | TutorialRoute String
  | UnknownRoute


type Region
  = Light
  | Heavy


pageToRegion : Page -> Region
pageToRegion page =
  case page of
    Tutorial _ ->
      Heavy

    _ ->
      Light


urlToRoute : Url.Url -> Route
urlToRoute url =
  let
    parser =
      Parser.oneOf
        [ Parser.map HomeRoute Parser.top
        , Parser.map HomeRoute (Parser.s "home")
        , Parser.map TutorialRoute (Parser.s "tutorial" </> tutorialName_)
        ]
  in
  Parser.parse parser url
    |> Maybe.withDefault UnknownRoute


route : Url.Url -> Model -> ( Model, Cmd Msg )
route url model =
  case urlToRoute url of
    HomeRoute ->
      stepHome model (Home.init ())

    TutorialRoute content ->
      stepTutorial model (Tutorial.init content)

    UnknownRoute ->
      ( { model | page = NotFound }, Cmd.none )


reloadIfNeeded : Url.Url -> Model -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
reloadIfNeeded url oldModel ( newModel, cmd ) =
  if pageToRegion newModel.page == pageToRegion oldModel.page then
    ( newModel, Cmd.batch [ cmd, Nav.pushUrl newModel.key (Url.toString url) ] )

  else
    -- Ignore the cmd because the page will be reloaded
    ( newModel, Nav.load (Url.toString url) )


tutorialName_ : Parser (String -> a) a
tutorialName_ =
  Parser.custom "TUTORIAL" Just


stepHome : Model -> ( Home.Model, Cmd Home.Msg ) -> ( Model, Cmd Msg )
stepHome model ( home, cmds ) =
  ( { model | page = Home home }
  , Cmd.map HomeMsg cmds
  )


stepTutorial : Model -> ( Tutorial.Model, Cmd Tutorial.Msg ) -> ( Model, Cmd Msg )
stepTutorial model ( tutorial, cmds ) =
  ( { model | page = Tutorial tutorial }
  , Cmd.map TutorialMsg cmds
  )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
  case model.page of
    NotFound ->
      { title = "AIWaffle"
      , body =
        [ Html.map NotFoundMsg <| NotFound.view {}
        ]
      }

    Home home ->
      { title = "AIWaffle"
      , body =
        [ Html.map HomeMsg <| Home.view home
        ]
      }

    Tutorial tutorial ->
      { title = Tutorial.getContentName tutorial.contentIndex
      , body =
        [ Html.map TutorialMsg <| Tutorial.view tutorial
        ]
      }

