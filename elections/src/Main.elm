module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)


-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- MODEL

type Model
  = Failure
  | Loading
  | Success String

init : () -> (Model, Cmd Msg)
init _ =
  (Loading, getElectionInfo)

-- UPDATE


type Msg = 
    GotElectionInfo (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    GotElectionInfo result ->
      case result of
        Ok url ->
          (Success url, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "Elections Coming Soon" ]
    , viewGif model
    ]


viewGif : Model -> Html Msg
viewGif model =
  case model of
    Failure ->
      div []
        [ text "I could not load the election info for some reason."
        ]

    Loading ->
      text "Loading..."

    Success url ->
      text "Add New Election Information here"



-- HTTP


getElectionInfo : Cmd Msg
getElectionInfo =
  Http.get
    { url = "https://www.googleapis.com/civicinfo/v2/elections?key=AIzaSyDntZTUcLkFXbB_aYmeGCf7U2CQkMYQ9gM"
    , expect = Http.expectJson GotGif gifDecoder -- replace gotgif with some kind of structure or something. Create a decoder method properly
    }


gifDecoder : Decoder String
gifDecoder =
  