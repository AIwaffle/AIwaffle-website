port module Page.Home exposing (main, Model, Msg, init, update, view)

import Browser
import Html exposing (Html)
import Html.Attributes
import Element as E exposing (Element)
import Element.Font as Font
import Element.Background as Background
import Element.Input as Input
import Element.Border as Border
import Style
import Constants exposing (courseNames, serverRoot)
import Http
import Json.Encode as Encode
import FeatherIcons

port resetContent : () -> Cmd msg

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Model =
  { courses : List String
  , username : String
  , password : String
  , loggedIn : Bool
  , popUp : PopUp
  }


type PopUp
  = LogInPopUp
  | LogInErrorPopUp
  | SignUpPopUp
  | SignUpErrorPopUp
  | NoPopUp


type Msg
  = ShowLogInPopUp
  | ShowSignUpPopUp
  | LogIn
  | LoggedIn (Result Http.Error ())
  | SignUp
  | SignedUp (Result Http.Error ())
  | ChangedUserName String
  | ChangedUserPassword String
  | ClosePopUp


subscriptions : Model -> Sub msg
subscriptions _ =
  Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
  ( { courses =
    courseNames
  , username =
    ""
  , password =
    ""
  , loggedIn =
    False
  , popUp =
    NoPopUp
  }
  , resetContent ()
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
    , E.padding 10
    , E.inFront (viewPopUp model)
    ]
    ( E.column
      [ E.width (E.fill |> E.maximum 800)
      , E.centerX
      ]
      [ viewHeader
      , viewBody model
      ]
    )


viewPopUp : Model -> Element Msg
viewPopUp model =
  case model.popUp of
    NoPopUp ->
      E.none
    
    LogInPopUp ->
      viewLogInPopUp model
    
    LogInErrorPopUp ->
      viewLogInErrorPopUp model
    
    SignUpPopUp ->
      viewSignUpPopUp model
    
    SignUpErrorPopUp ->
      viewSignUpErrorPopUp model


viewSignUpErrorPopUp model =
  viewBasePopUp
  [ title "Sign Up Error"
  , E.paragraph
    []
    [ E.text "I can't sign you up because your username has already been taken. Please make up another one."
    ]
  , Input.button
    [ Background.color Style.color.dark
    , Font.color Style.color.yellow
    , E.padding 10
    , E.alignRight
    ]
    { onPress =
      Just ShowSignUpPopUp
    , label =
      E.text "Try Again"
    }
  ]


viewSignUpPopUp : Model -> Element Msg
viewSignUpPopUp model =
  viewBasePopUp
  [ title "Sign Up"
  , Input.username
    []
    { onChange =
      ChangedUserName
    , text =
      model.username
    , placeholder =
      Nothing
    , label =
      Input.labelLeft [] <| E.text "Username: "
    }
  , Input.currentPassword
    []
    { onChange =
      ChangedUserPassword
    , text =
      model.password
    , placeholder =
      Nothing
    , label =
      Input.labelLeft [] <| E.text "Password: "
    , show =
      False
    }
  , Input.button
    [ Background.color Style.color.dark
    , Font.color Style.color.yellow
    , E.padding 10
    , E.alignRight
    ]
    { onPress =
      Just SignUp
    , label =
      E.text "Submit"
    }
  ]


viewLogInErrorPopUp : Model -> Element Msg
viewLogInErrorPopUp model =
  viewBasePopUp
  [ title "Log In Error"
  , E.paragraph []
    [ E.text "I can't log you in. Did you make a typo in your username and/or password? If you haven't signed up yet, sign up first."
    ]
  , E.row
    [ E.width E.fill ]
    [ Input.button
      [ Background.color Style.color.dark
      , Font.color Style.color.yellow
      , E.padding 10
      , E.alignLeft
      ]
      { onPress =
        Just ShowSignUpPopUp
      , label =
        E.text "Sign Up"
      }
    , Input.button
      [ Background.color Style.color.dark
      , Font.color Style.color.yellow
      , E.padding 10
      , E.alignRight
      ]
      { onPress =
        Just ShowLogInPopUp
      , label =
        E.text "Try Again"
      }
    ]
  ]


viewLogInPopUp : Model -> Element Msg
viewLogInPopUp model =
  viewBasePopUp
  [ title "Log In"
  , Input.username
    []
    { onChange =
      ChangedUserName
    , text =
      model.username
    , placeholder =
      Nothing
    , label =
      Input.labelLeft [] <| E.text "Username: "
    }
  , Input.currentPassword
    []
    { onChange =
      ChangedUserPassword
    , text =
      model.password
    , placeholder =
      Nothing
    , label =
      Input.labelLeft [] <| E.text "Password: "
    , show =
      False
    }
  , Input.button
    [ Background.color Style.color.dark
    , Font.color Style.color.yellow
    , E.padding 10
    , E.alignRight
    ]
    { onPress =
      Just LogIn
    , label =
      E.text "Submit"
    }
  ]


viewBasePopUp : List (Element Msg) -> Element Msg
viewBasePopUp elements =
  E.column
    [ E.centerX
    , E.centerY
    , E.width
      (E.fill
        |> E.maximum 400
      )
    , Background.color <| Style.color.yellow
    , Font.color <| Style.color.dark
    , E.spacing 20
    , E.padding 20
    , Border.shadow
      { offset =
        ( 3, 3 )
      , size =
        5
      , blur =
        5
      , color =
        Style.color.dark
      }
    , E.inFront viewClosePopUpButton
    ]
    elements


viewClosePopUpButton : Element Msg
viewClosePopUpButton =
  Input.button
    [ E.alignRight
    , E.padding 10
    ]
    { onPress =
      Just ClosePopUp
    , label =
      E.html (FeatherIcons.x |> FeatherIcons.toHtml [])
    }


viewHeader : Element Msg
viewHeader =
  E.row
    [ E.width E.fill
    , E.padding 10
    , E.spacing 20
    ]
    [ E.el
      [ E.alignLeft ] 
      ( E.text "AIwaffle"
      )
    , E.link [ E.alignLeft ]
      { url = "/about"
      , label = E.text "About"
      }
    , Input.button
      [ E.alignRight
      ]
      { onPress =
        Just ShowLogInPopUp
      , label =
        E.text "Log In"
      }
    , Input.button
      [ E.alignRight
      ]
      { onPress =
        Just ShowSignUpPopUp
      , label =
        E.text "Sign Up"
      }
    ]


viewBody : Model -> Element Msg
viewBody model =
  E.column
    [ E.width E.fill
    , E.spacing 20
    , E.padding 30
    ]
    [ E.row
      (E.centerX
        :: Style.font.title
      )
      [ E.image
        [ E.htmlAttribute <| Html.Attributes.class "inline-logo"
        ]
        { src = "/assets/logo.svg"
        , description = "AIwaffle Logo"
        }
      , E.paragraph [] [ E.text "Develop Your AI Superpower" ]
      ]
    , E.paragraph
      [ E.centerX
      , Font.center
      , E.width (E.fill |> E.maximum 600)
      ]
      [ E.text "learn AI Interactively from scratch for free. \nUnderstand the workings behind the AI superpower, \nand start using them yourself!"
      ]
    , E.wrappedRow
      [ E.padding 20
      , E.spacing 40
      , E.htmlAttribute <| Html.Attributes.style "margin-left" "auto"
      , E.htmlAttribute <| Html.Attributes.style "margin-right" "auto"
      , E.htmlAttribute <| Html.Attributes.style "display" "flex"
      , E.htmlAttribute <| Html.Attributes.style "justify-content" "center"
      ]
      ( List.map
        viewCourseCard
        model.courses
      )
    ]


viewCourseCard courseTitle =
  E.el
    [ Background.image "/assets/waffle.svg"
    , E.width <| E.px 200
    , E.height <| E.px 200
    ]
    ( E.paragraph
      [ Font.center
      , Font.color Style.color.dark
      , Font.bold
      , E.centerY
      ]
      [ E.link []
        { url = "/tutorial/" ++ courseTitle
        , label = E.text courseTitle
        }
      ]
      )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ShowLogInPopUp ->
      showLogInPopUp model
    
    ShowSignUpPopUp ->
      showSignUpPopUp model
    
    LogIn ->
      logIn model
    
    LoggedIn result ->
      loggedIn result model
    
    SignUp ->
      signUp model

    SignedUp result ->
      signedUp result model

    ChangedUserName newName ->
      changedUserName newName model
    
    ChangedUserPassword newPassword ->
      changedUserPassword newPassword model
    
    ClosePopUp ->
      closePopUp model


closePopUp : Model -> ( Model, Cmd Msg )
closePopUp model =
  ( { model
    | popUp =
      NoPopUp
  }
  , Cmd.none
  )


changedUserPassword : String -> Model -> ( Model, Cmd Msg )
changedUserPassword newPassword model =
  ( { model
    | password =
      newPassword
  }
  , Cmd.none
  )


changedUserName : String -> Model -> ( Model, Cmd Msg )
changedUserName newName model =
  ( { model
    | username =
      newName
  }
  , Cmd.none
  )


loggedIn : Result Http.Error ()-> Model -> ( Model, Cmd Msg )
loggedIn result model =
  ( case result of
    Ok _ ->
      { model
        | loggedIn =
          True
      }
    
    Err _ ->
      { model
        | popUp =
          LogInErrorPopUp
      }
  , Cmd.none
  )



signedUp : Result Http.Error ()-> Model -> ( Model, Cmd Msg )
signedUp result model =
  case result of
    Ok _ ->
      logIn model
    
    Err err ->
      let
        _ =
          Debug.log "sign up error" err
      in
      ( { model
        | popUp =
          SignUpErrorPopUp
      }
      , Cmd.none
      )


showSignUpPopUp : Model -> ( Model, Cmd Msg )
showSignUpPopUp model =
  ( { model
    | popUp =
      SignUpPopUp
  }
  , Cmd.none
  )


showLogInPopUp : Model -> ( Model, Cmd Msg )
showLogInPopUp model =
  ( { model
    | popUp =
      LogInPopUp
  }
  , Cmd.none
  )


signUp : Model -> ( Model, Cmd Msg )
signUp model =
  ( model
  , Http.post
      { url = serverRoot ++ "auth/register"
      , body = Http.jsonBody <| Encode.object
        [ ( "username", Encode.string model.username )
        , ( "password", Encode.string model.password )
        ]
      , expect = Http.expectWhatever SignedUp
    }
  )


logIn : Model -> ( Model, Cmd Msg )
logIn model =
  ( model
  , Http.post
      { url = serverRoot ++ "auth/login"
      , body = Http.jsonBody <| Encode.object
        [ ( "username", Encode.string model.username )
        , ( "password", Encode.string model.password )
        ]
      , expect = Http.expectWhatever LoggedIn
    }
  )


title : String -> Element Msg
title text =
  E.el
  [ Font.size 26
  , Font.bold
  , E.paddingEach
    { top =
      10
    , bottom =
      10
    , left =
      0
    , right =
      0
    }
  ]
  (E.text text)
