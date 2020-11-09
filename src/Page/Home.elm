port module Page.Home exposing (Model, Msg, init, update, view)

import Browser
import Constants exposing (courseIds, courseNames, markdownCourseIds, serverRoot)
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Field as Field
import Json.Encode as Encode
import SharedState exposing (SharedState, UpdateSharedState(..))
import Style


port resetContent : () -> Cmd msg


type alias Model =
    { courses : List ( String, String )
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
    | LogOut
    | LoggedOut (Result Http.Error ())
    | SignUp
    | SignedUp (Result Http.Error AuthResponse)
    | ChangedUserName String
    | ChangedUserPassword String
    | ClosePopUp


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


init : () -> ( Model, Cmd Msg, UpdateSharedState )
init _ =
    ( { courses =
            List.map2 Tuple.pair courseIds courseNames
      , popUp =
            NoPopUp
      }
    , resetContent ()
    , NoUpdate
    )


view : SharedState -> Model -> Html Msg
view sharedState model =
    E.layout
        [ Font.family
            [ Font.typeface "Nunito"
            , Font.sansSerif
            ]
        , Font.color <| Style.color.yellow
        , Font.glow (E.rgba255 255 218 94 0.8) 3
        , Background.color <| Style.color.dark
        , E.padding 10
        , E.inFront (viewPopUp sharedState model)
        ]
        (E.column
            [ E.width (E.fill |> E.maximum 800)
            , E.centerX
            ]
            [ viewHeader sharedState model
            , viewBody sharedState model
            ]
        )


viewPopUp : SharedState -> Model -> Element Msg
viewPopUp sharedState model =
    case model.popUp of
        NoPopUp ->
            E.none

        LogInPopUp ->
            viewLogInPopUp sharedState model

        LogInErrorPopUp reason ->
            viewLogInErrorPopUp reason model

        SignUpPopUp ->
            viewSignUpPopUp sharedState model

        SignUpErrorPopUp reason ->
            viewSignUpErrorPopUp reason model


viewSignUpErrorPopUp : String -> Model -> Element Msg
viewSignUpErrorPopUp reason model =
    viewBasePopUp
        [ title "Sign up Error"
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


viewSignUpPopUp : SharedState -> Model -> Element Msg
viewSignUpPopUp sharedState model =
    viewBasePopUp
        [ title "Sign up"
        , Input.username
            []
            { onChange =
                ChangedUserName
            , text =
                sharedState.username
            , placeholder =
                Nothing
            , label =
                Input.labelLeft [] <| E.text "Username: "
            }
        , Input.currentPassword
            [ onEnter SignUp ]
            { onChange =
                ChangedUserPassword
            , text =
                sharedState.password
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


onEnter : msg -> E.Attribute msg
onEnter msg =
    E.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


viewLogInErrorPopUp : String -> Model -> Element Msg
viewLogInErrorPopUp reason model =
    viewBasePopUp
        [ title "Login Error"
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
                    E.text "Sign up"
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


viewLogInPopUp : SharedState -> Model -> Element Msg
viewLogInPopUp sharedState model =
    viewBasePopUp
        [ title "Login"
        , Input.username
            []
            { onChange =
                ChangedUserName
            , text =
                sharedState.username
            , placeholder =
                Nothing
            , label =
                Input.labelLeft [] <| E.text "Username: "
            }
        , Input.currentPassword
            [ onEnter LogIn ]
            { onChange =
                ChangedUserPassword
            , text =
                sharedState.password
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


viewHeader : SharedState -> Model -> Element Msg
viewHeader sharedState model =
    E.row
        [ E.width E.fill
        , E.padding 10
        , E.spacing 20
        ]
        [ E.el
            [ E.alignLeft ]
            (E.text "AIwaffle")
        , E.link
            [ E.alignLeft
            , Font.underline
            ]
            { url = "/about"
            , label = E.text "About"
            }
        , E.el [ E.alignRight ] <|
            if sharedState.loggedIn then
                E.row
                    [ E.padding 10
                    , E.spacing 20
                    ]
                    [ E.text sharedState.username
                    , Input.button
                        [ Font.underline ]
                        { onPress =
                            Just <| LogOut
                        , label =
                            E.text "Logout"
                        }
                    ]

            else
                Input.button
                    [ Font.underline ]
                    { onPress =
                        Just ShowLogInPopUp
                    , label =
                        E.text "Login"
                    }
        , if sharedState.loggedIn then
            E.none

          else
            Input.button
                [ E.alignRight
                , Font.underline
                ]
                { onPress =
                    Just ShowSignUpPopUp
                , label =
                    E.text "Sign up"
                }
        ]


viewBody : SharedState -> Model -> Element Msg
viewBody sharedState model =
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
            (List.map
                (viewCourseCard sharedState)
                model.courses
            )
        ]


viewCourseCard sharedState ( courseId, courseName ) =
    E.el
        [ Background.image "/assets/waffle.svg"
        , E.width <| E.px 200
        , E.height <| E.px 200
        ]
        (E.paragraph
            [ Font.center
            , Font.color Style.color.dark
            , Font.bold
            , E.centerY
            ]
            [ E.newTabLink
                [ Font.underline ]
                { url =
                    if
                        not sharedState.loggedIn
                            || List.member courseId markdownCourseIds
                    then
                        "/tutorial/" ++ courseId

                    else
                        "/jhub/user/" ++ sharedState.username ++ "/notebooks/Courses/" ++ courseId ++ ".ipynb"
                , label = E.text courseName
                }
            ]
        )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, UpdateSharedState )
update sharedState msg model =
    case msg of
        ShowLogInPopUp ->
            showLogInPopUp model

        ShowSignUpPopUp ->
            showSignUpPopUp model

        LogIn ->
            logIn sharedState model

        LoggedIn result ->
            loggedIn result model
        
        LogOut ->
            logOut sharedState model

        LoggedOut result ->
            loggedOut result model

        SignUp ->
            signUp sharedState model

        SignedUp result ->
            signedUp sharedState result model

        ChangedUserName newName ->
            changedUserName newName model

        ChangedUserPassword newPassword ->
            changedUserPassword newPassword model

        ClosePopUp ->
            closePopUp model


closePopUp : Model -> ( Model, Cmd Msg, UpdateSharedState )
closePopUp model =
    ( { model
        | popUp =
            NoPopUp
      }
    , Cmd.none
    , NoUpdate
    )


changedUserPassword : String -> Model -> ( Model, Cmd Msg, UpdateSharedState )
changedUserPassword newPassword model =
    ( model
    , Cmd.none
    , UpdatePassword newPassword
    )


changedUserName : String -> Model -> ( Model, Cmd Msg, UpdateSharedState )
changedUserName newName model =
    ( model
    , Cmd.none
    , UpdateUsername newName
    )


loggedIn : Result Http.Error AuthResponse -> Model -> ( Model, Cmd Msg, UpdateSharedState )
loggedIn result model =
    case result of
        Ok { success, reason } ->
            ( { model
                | popUp =
                    if success then
                        NoPopUp

                    else
                        LogInErrorPopUp reason
              }
            , Cmd.none
            , UpdateLoggedIn True
            )

        Err err ->
            let
                _ =
                    Debug.log "login error" err
            in
            ( { model
                | popUp =
                    LogInErrorPopUp "AIwaffle server or your network connection has some problem. Please try logging in again."
              }
            , Cmd.none
            , UpdateLoggedIn False
            )


loggedOut : Result Http.Error () -> Model -> ( Model, Cmd Msg, UpdateSharedState )
loggedOut result model =
    ( model
    , Cmd.none
    , UpdateLoggedIn False
    )


signedUp : SharedState -> Result Http.Error AuthResponse -> Model -> ( Model, Cmd Msg, UpdateSharedState )
signedUp sharedState result model =
    case result of
        Ok { success, reason } ->
            if success then
                logIn sharedState model

            else
                ( { model
                    | popUp =
                        SignUpErrorPopUp reason
                  }
                , Cmd.none
                , NoUpdate
                )

        Err err ->
            let
                _ =
                    Debug.log "sign up error" err
            in
            ( { model
                | popUp =
                    SignUpErrorPopUp "AIwaffle server or your network connection has some problem. Please try signing up again."
              }
            , Cmd.none
            , NoUpdate
            )


showSignUpPopUp : Model -> ( Model, Cmd Msg, UpdateSharedState )
showSignUpPopUp model =
    ( { model
        | popUp =
            SignUpPopUp
      }
    , Cmd.none
    , NoUpdate
    )


showLogInPopUp : Model -> ( Model, Cmd Msg, UpdateSharedState )
showLogInPopUp model =
    ( { model
        | popUp =
            LogInPopUp
      }
    , Cmd.none
    , NoUpdate
    )


type alias AuthResponse =
    { success : Bool
    , reason : String
    }


signUp : SharedState -> Model -> ( Model, Cmd Msg, UpdateSharedState )
signUp sharedState model =
    ( model
    , Http.post
        { url = serverRoot ++ "api/auth/register"
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "username", Encode.string sharedState.username )
                    , ( "password", Encode.string sharedState.password )
                    ]
        , expect = Http.expectJson SignedUp authResponseDecoder
        }
    , NoUpdate
    )


logIn : SharedState -> Model -> ( Model, Cmd Msg, UpdateSharedState )
logIn sharedState model =
    ( model
    , Http.post
        { url = serverRoot ++ "api/auth/login"
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "username", Encode.string sharedState.username )
                    , ( "password", Encode.string sharedState.password )
                    , ( "session", Encode.int 1 ) -- store the login in session
                    ]
        , expect = Http.expectJson LoggedIn authResponseDecoder
        }
    , NoUpdate
    )


logOut : SharedState -> Model -> ( Model, Cmd Msg, UpdateSharedState )
logOut sharedState model =
    ( model
    , Http.post
        { url = serverRoot ++ "auth/logout"
        , body =
            Http.emptyBody
        , expect = Http.expectWhatever LoggedOut
        }
    , NoUpdate
    )


authResponseDecoder : Decoder AuthResponse
authResponseDecoder =
    Field.require "success" Decode.bool <|
        \success ->
            Field.attempt "reason" Decode.string <|
                \reason ->
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
