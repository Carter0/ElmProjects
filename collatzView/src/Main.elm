module Main exposing (..)

import Browser
import Html exposing (Html, br, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    Result String Int


init : Model
init =
    Result.Ok 0



-- UPDATE


type Msg
    = Input String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input input ->
            if String.isEmpty <| String.filter Char.isDigit input then
                Result.Err "Only number inputs allowed"

            else if String.length input > 8 then
                Result.Err "Limit of 8 numbers"

            else
                collatz <| Maybe.withDefault 0 <| String.toInt input


collatz : Int -> Result String Int
collatz start =
    if start <= 0 then
        Result.Err "Only positive numbers are allowed"

    else
        Result.Ok (collatzHelper start 0)


collatzHelper : Int -> Int -> Int
collatzHelper start counter =
    if start == 1 then
        counter

    else if modBy 2 start == 0 then
        collatzHelper (start // 2) (counter + 1)

    else
        collatzHelper (3 * start + 1) (counter + 1)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text "The Collatz Conjecture or 3x+1 problem can be summarized as follows: Take any positive integer n."
            , br [] []
            , text "If n is even, divide n by 2 to get n / 2. If n is odd, multiply n by 3 and add 1 to get 3n + 1."
            , br [] []
            , text "Repeat the process indefinitely. The conjecture states that no matter which number you start with, you will always reach 1 eventually."
            , br [] []
            , text "Given a number n, return the number of steps required to reach 1."
            , br [] []
            , br [] []
            ]
        , div []
            [ text <|
                case model of
                    Err err ->
                        err
                        
                    Ok num ->
                        String.fromInt num
            ]
        , input [ placeholder "Enter number", onInput Input ] []
        ]


{-
Wrap it all in a div so you dont have to deal with the annoying curser refresh change issue.
-}
