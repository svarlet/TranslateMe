module Main exposing (..)

import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import CsvParser exposing (parseCsv)

-- MODEL

type alias Model =
    { score : Int, words : List (List String), word : String, input : String }

init : ( Model, Cmd Msg)
init =
    (Model 0 [] "" "", downloadWords)

fileUrl : String
fileUrl =
    "https://dl.dropboxusercontent.com/u/4800046/word-list.csv"

downloadWords : Cmd Msg
downloadWords =
    fileUrl
        |> Http.getString
        |> Http.send Downloaded

-- UPDATE

type Msg
    = UpdateInput String
    | Downloaded (Result Http.Error String)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInput newInput ->
            ( { model | input = newInput }, Cmd.none )
        Downloaded (Ok csv) ->
            ( { model | words = CsvParser.parseCsv csv }, Cmd.none )
        Downloaded (Err _) ->
            ( model, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
    let
        question = "Translate " ++ model.word
    in
        div
          []
          [ text question
          , input [ placeholder "Ma réponse", onInput UpdateInput] []
          ]

-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
