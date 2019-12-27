module Main exposing (isPaired)

import Browser
import Html exposing (Html, br, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)




main =
    Browser.sandbox { init = init, update = update, view = view }



type alias Model = 
      Bool



type Msg = 
    Input String


init : Model 
init = 
    True

update : Msg -> Model -> Model 
update msg model =
    case msg of 
        Input input ->
            isPaired input



isPaired : String -> Bool
isPaired input =
    case String.foldl stackFunctionality (Result.Ok []) input of
        Err error ->
            False

        Ok value ->
            List.isEmpty value

stackFunctionality : Char -> Result String (List Char) -> Result String (List Char) 
stackFunctionality bracket bracketStack =
    Result.andThen (updateStack bracket) bracketStack

updateStack : Char -> List Char -> Result String (List Char)
updateStack bracket bracketStack =
    if isStartingBrackets bracket then
        Result.Ok <| bracket :: bracketStack

    else if isEndingBrackets bracket then
        let
            popValue =
                List.head bracketStack
        in
        case popValue of
            Nothing ->
                Result.Err "Nothing to pop off in stack"

            Just a ->
                if bracketToLookFor a == bracket then
                    Result.Ok <| List.drop 1 bracketStack

                else
                    Result.Err "Brackets do not match"

    else
        Result.Ok bracketStack


isEndingBrackets : Char -> Bool
isEndingBrackets char =
    char == '}' || char == ']' || char == ')'


isStartingBrackets : Char -> Bool
isStartingBrackets char =
    char == '{' || char == '[' || char == '('


bracketToLookFor : Char -> Char
bracketToLookFor char =
    if char == '{' then
        '}'

    else if char == '(' then
        ')'

    else
        ']'



view : Model -> Html Msg
view model =
    div [] [
        bracketDescription
        , text <| case model of
           True -> "They Match!"
           False -> "They do not match."
        , div [] [
            input [placeholder "Enter brackets", onInput Input] []
        ]
    ]


bracketDescription : Html Msg  
bracketDescription =
    div [] [
        text "Given a string containing brackets [], braces {}, parentheses (), or any combination thereof,"
        , br [] []
        , text "verify that any and all pairs are matched and nested correctly."
        , br [] []
        , br [] []
    ]