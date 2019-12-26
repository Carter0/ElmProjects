module Main exposing (..)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    String


init : Model
init =
    "Hello, World!"


type alias Msg =
    String


update : Msg -> Model -> Model
update msg model =
    msg


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Input new string", value model, onInput identity ] []
        , div [] [ text model ]
        ]
