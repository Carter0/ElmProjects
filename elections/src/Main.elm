module Main exposing (..)

import Browser
import Element exposing (Element, padding, centerX, centerY, column, el, spacing, text, rgba255, fill)
import Element.Font as Font
import Element.Border as Border
import Element.Background as Background
import Html exposing (Html)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, list, map4, string)



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
    | Success (List ElectionData)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getElectionInfo )



-- UPDATE


type Msg
    = GotElectionInfo (Result Http.Error (List ElectionData))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotElectionInfo result ->
            case result of
                Ok electionData ->
                    ( Success electionData, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout []
        (case model of
            Loading ->
                text "Loading upcoming elections..."

            Failure ->
                loadFailureElement

            Success electionList ->
                electionColumn electionList
        )




loadFailureElement : Element msg
loadFailureElement =
    el []
        (text "Failed to load the election data.")


electionColumn : List ElectionData -> Element msg
electionColumn electionList =
    column [ spacing 30 ]
        ([headerElement] 
        ++
        (List.map electionElement electionList))

headerElement : Element msg 
headerElement = 
    el [Font.bold, (Font.size 20), (Background.color <| rgba255 133 168 195 255), (Element.width fill), padding 40] 
        (text "Upcoming Elections in the United States")

electionElement : ElectionData -> Element msg
electionElement election =
    column [ (Background.color <| rgba255 133 168 195 255), Border.solid, (Border.rounded 5), padding 10 ]
        [ el [ Font.bold ] (text election.name)
        , el [] (text election.electionDay)
        ]



-- HTTP


getElectionInfo : Cmd Msg
getElectionInfo =
    Http.get
        { url = "https://www.googleapis.com/civicinfo/v2/elections?key=AIzaSyDntZTUcLkFXbB_aYmeGCf7U2CQkMYQ9gM"
        , expect = Http.expectJson GotElectionInfo electionDecoder
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
    map4 ElectionData
        (field "id" string)
        (field "name" string)
        (field "electionDay" string)
        (field "ocdDivisionId" string)
