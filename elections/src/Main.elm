module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, field, string, int, map4, list)


-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- MODEL
-- Also see election data down below.
type Model
  = Failure
  | Loading
  | Success (List ElectionData)



init : () -> (Model, Cmd Msg)
init _ =
  (Loading, getElectionInfo)

-- UPDATE


type Msg = 
    GotElectionInfo (Result Http.Error (List ElectionData))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        GotElectionInfo result -> 
            case result of
                Ok electionData ->
                    (Success electionData, Cmd.none)

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
    , viewElections model
    ]


viewElections : Model -> Html Msg
viewElections model =
  case model of
    Failure ->
      div []
        [ text "I could not load the election info for some reason." ]

    Loading ->
      text "Loading..."

    Success electionInfo ->
            ul [] (List.map viewElection electionInfo)
            

viewElection : ElectionData -> Html Msg
viewElection election = 
    div [] 
        [ div [] [text ("The election is called " ++ election.name)]
        , div [] [text ("The election day is " ++ election.electionDay)]
        ]
      



-- HTTP


getElectionInfo : Cmd Msg
getElectionInfo =
  Http.get
    { url = "https://www.googleapis.com/civicinfo/v2/elections?key=AIzaSyDntZTUcLkFXbB_aYmeGCf7U2CQkMYQ9gM"
    , expect = Http.expectJson GotElectionInfo electionDecoder -- replace  with some kind of structure or something. Create a decoder method properly
    }


type alias ElectionData = 
    { id : String
    , name : String
    , electionDay : String
    , ocdDivisionId : String
    }


-- get the elections field
electionDecoder : Decoder (List ElectionData)
electionDecoder =
    field "elections" 
        (list electionDecoderHelper)

electionDecoderHelper : Decoder ElectionData
electionDecoderHelper = 
    (map4 ElectionData
            (field "id" string)
            (field "name" string)
            (field "electionDay" string)
            (field "ocdDivisionId" string))
