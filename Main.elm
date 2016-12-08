module Main exposing (..)

import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import CsvParser exposing (parseCsv)
import Random exposing (generate, int)
import List.Extra exposing (getAt)

-- MODEL

type BootstrapStatus
    = Loading
    | Success
    | Failure
    | Ready

type alias Translation = List String

type alias Model =
    { bootstrapStatus : BootstrapStatus
    , score : Int
    , words : List (List String)
    , translation : Translation
    , input : String
    }

init : ( Model, Cmd Msg)
init =
    (Model Loading 0 [] [] "", downloadWords)

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
    | RandomIndexPicked Int

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInput newInput ->
            ( { model | input = newInput }
            , Cmd.none
            )
        Downloaded (Ok csv) ->
            ( { model
                  | words = CsvParser.parseCsv csv
                  , bootstrapStatus = Success
              }
            , Random.generate RandomIndexPicked (Random.int 0 ((List.length model.words) - 1))
            )
        Downloaded (Err _) ->
            ( { model | bootstrapStatus = Failure }
            , Cmd.none
            )
        RandomIndexPicked index ->
            case List.Extra.getAt index model.words of
                Just aTranslation ->
                    ( { model
                          | translation = aTranslation
                          , bootstrapStatus = Ready
                      }
                    , Cmd.none
                    )
                Nothing ->
                    ( model, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
    case model.bootstrapStatus of
        Loading ->
            text "Wait while the list of words are being loaded."

        Ready ->
            let
                question =
                    model.translation
                        |> List.head
                        |> Maybe.map ((++) "Translate ")
                        |> Maybe.withDefault "An error happened :("
            in
                div
                []
                [ text question
                , input [ placeholder "Ma réponse", onInput UpdateInput] []
                ]

        Success ->
            text "Words loaded successfully :)"

        Failure ->
            text "Loading the list of words failed."

-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
