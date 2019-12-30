module Main exposing (..)

import Browser
import Html exposing (Html, div, text, input, button, p, h3, br)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model = 
     {inNumber : String
    , inBase : Int
    , outBase : Int
    , outNumber : Maybe String }


init : Model
init =
  {inNumber = "42", inBase = 2, outBase = 10, outNumber = Just "0"}


-- UPDATE


type Msg
  = InBase String 
  | OutBase String 
  | InNumber String
  | Calculate 


update : Msg -> Model -> Model
update msg model =
    let
        newBase base = 
            Maybe.withDefault 0 <| String.toInt base
    in
    case msg of
        InBase inBase ->
            {model |  inBase = newBase inBase  }
        
        OutBase outBase -> 
            {model |  outBase = newBase outBase }
        
        InNumber number -> 
            {model | inNumber = String.filter Char.isDigit number }
        
        Calculate -> 
            { model | outNumber = rebase model.inBase (toDigitList model.inNumber []) model.outBase }


toDigitList : String -> List Int -> List Int
toDigitList string acc = 
    if String.isEmpty string then 
        List.reverse acc
    else 
       toDigitList (String.dropLeft 1 string) ((Maybe.withDefault 0 <| String.toInt <| String.left 1 string) :: acc)

rebase : Int -> List Int -> Int -> Maybe String
rebase inBase digits outBase =
    let
        toString input = 
            input 
                |> List.map String.fromInt
                |> List.foldr (++) ""
    in
    
    if badConditions inBase digits outBase == True then
        Nothing

    else
        Just (base10Conversion (List.length digits - 1) inBase digits 0 
            |> outBaseConversion outBase []
            |> toString)


badConditions : Int -> List Int -> Int -> Bool
badConditions inBase digits outBase =
    List.isEmpty (List.filter (\x -> x /= 0) digits)
        || inBase
        <= 1
        || outBase
        <= 1
        || List.any (\x -> x < 0) digits
        || List.any (\x -> x >= inBase) digits


base10Conversion : Int -> Int -> List Int -> Int -> Int
base10Conversion position base digits sum =
    case digits of
        [] ->
            sum

        x :: xs ->
            base10Conversion (position - 1) base xs (sum + (x * base ^ position))


outBaseConversion : Int -> List Int -> Int -> List Int
outBaseConversion outBase acc number =
    if number == 0 then
        acc

    else
        outBaseConversion outBase (modBy outBase number :: acc) (number // outBase)


-- VIEW

view : Model -> Html Msg
view model =
    div [] [
        description
        , inputs model
        ]

inputs : Model -> Html Msg 
inputs model = 
  div [] [
      div [] [input [placeholder "Enter Number to Convert", onInput InNumber] []
      , div [] [input [placeholder "Enter InBase", onInput InBase] [], input [placeholder "Enter OutBase", onInput OutBase][]] 
      , div [] [button [onClick Calculate] [text "calculate"]]
      , text <| Maybe.withDefault "Invalid input" model.outNumber]
  ]

description : Html Msg 
description = 
    div [] [
        div [] [h3 [] [text "General Base Conversion"]]
        , p [] [
            text "Convert a number, represented as a sequence of digits in one base, to any other base."
            , br [] []
            , text "Will convert anything from base 2 to base 10."
            , br [] []
            , text "Input a number and its inbase and outbase."
            , br [] []
            , text "The inbase represents the starting base and the outbase represents the base you want to convert to. " 
            , br [] []
            , text "For example, when converting 42 base 10 to base 2, the number you want to convert is 42, the inbase is 10 and the outbase is base 2."
            ]
        ]