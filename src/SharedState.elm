module SharedState exposing (SharedState, UpdateSharedState(..), update)


type alias SharedState =
    { username : String
    , password : String
    , loggedIn : Bool
    }


type UpdateSharedState
    = UpdateUsername String
    | UpdatePassword String
    | UpdateLoggedIn Bool
    | NoUpdate


update : SharedState -> UpdateSharedState -> SharedState
update sharedState updateSharedState =
    case updateSharedState of
        UpdateUsername newUsername ->
            { sharedState
                | username =
                    newUsername
            }

        UpdatePassword newPassword ->
            { sharedState
                | password =
                    newPassword
            }

        UpdateLoggedIn newLoggedIn ->
            { sharedState
                | loggedIn =
                    newLoggedIn
            }

        NoUpdate ->
            sharedState
