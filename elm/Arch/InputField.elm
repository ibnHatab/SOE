module InputField where

import Html exposing (Html, Attribute, div, input, span, text, toElement, button)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue, onClick)
import Signal exposing (Address, Mailbox, forwardTo)

import Effects exposing (Effects, none)
import Task exposing (Task)
import StartApp
import String
import Debug


-- MODEL

type alias Model = {
    name : String
  }


empty : Model
empty =
  Model ""


-- UPDATE

type Action
    = Name String
    | Reset


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action |> Debug.log "action" of
    Name name ->
      ({ model | name <- name }, Effects.none)
    Reset ->
      ({ model | name <- "" }, Effects.none)


-- VIEW
view : Address Action -> Model -> Html
view address model =
  div []
        [ field "text" address Name "User Name" model.name
        , button [ onClick address Reset ] [ text "BTN" ]
        ]

field : String -> Address Action -> (String -> Action) -> String -> String -> Html
field fieldType address toAction name content =
  div []
    [ div [fieldNameStyle "160px"] [text name]
    , input
        [ type' fieldType
        , placeholder name
        , value content
        , on "input" targetValue (\string -> Signal.message address (toAction string))
        ]
        []
    ]


fieldNameStyle : String -> Attribute
fieldNameStyle px =
  style
    [ ("width", px)
    , ("padding", "10px")
    , ("text-align", "right")
    , ("display", "inline-block")
    ]


-- APPLICATION
app =
    StartApp.start
        { init = ( empty, Effects.none )
        , update = update
        , view = view
        , inputs = []
        }

main : Signal Html
main = app.html

port tasks : Signal (Task Effects.Never ())
port tasks =
    app.tasks
