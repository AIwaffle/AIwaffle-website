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
import Constants exposing (courseIds, courseNames, serverRoot)
import Http
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Field as Field
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
  { courses : List (String, String)
  , username : String
  , password : String
  , loggedIn : Bool
  , popUp : PopUp
  }


type PopUp
  = LogInPopUp
  | LogInErrorPopUp String
  | SignUpPopUp
  | SignUpErrorPopUp String
  | NoPopUp


type Msg
  = ShowLogInPopUp
  | ShowSignUpPopUp
  | LogIn
  | LoggedIn (Result Http.Error AuthResponse)
  | SignUp
  | SignedUp (Result Http.Error AuthResponse)
  | ChangedUserName String
  | ChangedUserPassword String
  | ClosePopUp


subscriptions : Model -> Sub msg
subscriptions _ =
  Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
  ( { courses =
    List.map2 Tuple.pair courseIds courseNames
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
      [ viewHeader model
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
    
    LogInErrorPopUp reason ->
      viewLogInErrorPopUp reason model
    
    SignUpPopUp ->
      viewSignUpPopUp model
    
    SignUpErrorPopUp reason ->
      viewSignUpErrorPopUp reason model


viewSignUpErrorPopUp : String -> Model -> Element Msg
viewSignUpErrorPopUp reason model =
  viewBasePopUp
  [ title "Sign Up Error"
  , E.paragraph
    []
    [ E.text reason
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


viewLogInErrorPopUp : String -> Model -> Element Msg
viewLogInErrorPopUp reason model =
  viewBasePopUp
  [ title "Log In Error"
  , E.paragraph []
    [ E.text reason
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


viewHeader : Model -> Element Msg
viewHeader model =
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
    , E.el [ E.alignRight ] <|
      if model.loggedIn then
        E.text model.username
      else
        Input.button []
          { onPress =
            Just ShowLogInPopUp
          , label =
            E.text "Log In"
          }
    , if model.loggedIn then
      E.none
    else
      Input.button
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


viewCourseCard (courseId, courseName) =
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
        { url = "/tutorial/" ++ courseId
        , label = E.text courseName
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


loggedIn : Result Http.Error AuthResponse -> Model -> ( Model, Cmd Msg )
loggedIn result model =
  ( case result of
    Ok { success, reason} ->
      { model
        | loggedIn =
          success
        , popUp =
          if success then
            NoPopUp
          else
            LogInErrorPopUp reason
      }
    
    Err err ->
      let
        _ = Debug.log "log in error" err
      in
      { model
        | popUp =
          LogInErrorPopUp "AIwaffle server or your network connection has some problem. Please try logging in again."
      }
  , Cmd.none
  )



signedUp : Result Http.Error AuthResponse -> Model -> ( Model, Cmd Msg )
signedUp result model =
  case result of
    Ok { success, reason } ->
      if success then
        logIn model
      else
        ( { model
          | popUp =
            SignUpErrorPopUp reason
        }
        , Cmd.none
        )
    
    Err err ->
      let
        _ = Debug.log "sign up error" err
      in
      ( { model
        | popUp =
          SignUpErrorPopUp "AIwaffle server or your network connection has some problem. Please try signing up again."
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


type alias AuthResponse =
  { success : Bool
  , reason : String
  }


signUp : Model -> ( Model, Cmd Msg )
signUp model =
  ( model
  , Http.post
      { url = serverRoot ++ "api/auth/register"
      , body = Http.jsonBody <| Encode.object
        [ ( "username", Encode.string model.username )
        , ( "password", Encode.string model.password )
        ]
      , expect = Http.expectJson SignedUp authResponseDecoder
    }
  )


logIn : Model -> ( Model, Cmd Msg )
logIn model =
  ( model
  , Http.post
      { url = serverRoot ++ "api/auth/login"
      , body = Http.jsonBody <| Encode.object
        [ ( "username", Encode.string model.username )
        , ( "password", Encode.string model.password )
        , ( "session", Encode.int 1 ) -- store the login in session
        ]
      , expect = Http.expectJson LoggedIn authResponseDecoder
    }
  )


authResponseDecoder : Decoder AuthResponse
authResponseDecoder =
  Field.require "success" Decode.bool <| \success ->
  Field.attempt "reason" Decode.string <| \reason ->

  Decode.succeed
    { success =
      success
    , reason =
      Maybe.withDefault "" reason
    }


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
