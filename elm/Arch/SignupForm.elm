module SignupForm where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Signal exposing (..)

import StartApp exposing (start)
import Effects
import Debug
import Task exposing (Task)
import Http
import Json.Decode exposing (succeed)

-- MODEL

initialModel =
  {username = "", password = "", errors = initialErrors}

initialErrors =
  { username = "", password = "", usernameTaken = False }

getErrors model =
    { username =
        if model.username == "" then
            "Please enter a username!"
        else
            ""
    , password =
        if model.password == "" then
            "Please enter a password!"
        else
            ""
    , usernameTaken = model.errors.usernameTaken
    }

-- UPDATE

update action model =
    Debug.log "update" <|
    if action.actionType == "VALIDATE" then
      let
        url = "https://api.github.com/users/" ++ model.username

        usernameTakenAction =
            { actionType = "USERNAME_TAKEN", payload = "" }

        usernameAvailableAction =
            { actionType = "USERNAME_AVAILABLE", payload = "" }

        request =
          Http.get (succeed usernameTakenAction) url

        neverFailingRequest =
          Task.onError request (\err -> Task.succeed usernameAvailableAction)
    in
      ({ model | errors <- getErrors model }, Effects.task neverFailingRequest)

    else if action.actionType == "SET_USERNAME" then
           ( { model | username <- action.payload }, Effects.none )
    else if action.actionType == "SET_PASSWORD" then
           ( { model | password <- action.payload }, Effects.none )

    else if action.actionType == "USERNAME_TAKEN" then
        ( withUsernameTaken True model, Effects.none )
    else if action.actionType == "USERNAME_AVAILABLE" then
        ( withUsernameTaken False model, Effects.none )

    else
      ( model, Effects.none )

withUsernameTaken isTaken model =
  let
    currentErrors = model.errors
    newErrors = { currentErrors | usernameTaken <- isTaken }
  in
    { model | errors <- newErrors }


-- VIEW
-- view : Action -> Model -> Html
view actionDispatcher model =
  Html.form [ id "signup-form" ]
         [ h1 [] [ text "Sensational Signup Form" ]

         , label [ for "username-field" ] [ text "username: " ]
         , input [ id "username-field"
                 , type' "text"
                 , value model.username
                 , on "input" targetValue (\str -> Signal.message
                                                   actionDispatcher { actionType = "SET_USERNAME"
                                                                    , payload = str })
                 ] []
         , div [ class "validation-error" ] [ text model.errors.username ]

         , label [ for "password" ] [ text "password: " ]

         , input [ id "password-field"
                 , type' "password"
                 , value model.password
                 , on "input" targetValue (\str -> Signal.message
                                                   actionDispatcher { actionType = "SET_PASSWORD"
                                                                    , payload = str })
                 ] []

         , div [ class "validation-error" ] [ text model.errors.password ]

         , div [ class "signup-button",
                       onClick actionDispatcher { actionType = "VALIDATE", payload = "" } ] [ text "Sign Up!" ]

         , div [ class "validation-error" ] [ text (viewUsernameErrors model) ]
         ]

viewUsernameErrors model =
   if model.errors.usernameTaken then
       "That username is taken!"
   else
       model.errors.username

app =
    StartApp.start
        { init = ( initialModel, Effects.none )
        , update = update
        , view = view
        , inputs = []
        }

main : Signal Html
main = app.html

port tasks : Signal (Task Effects.Never ())
port tasks =
    app.tasks
