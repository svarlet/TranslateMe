module Main exposing (..)

import Html exposing (Html, Attribute, div, input, text, button)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput, onClick)
import Http exposing (Error(..))
import List.Nonempty exposing (Nonempty(..))
import Platform.Sub exposing (..)
import Random
import Random.List
import Regex exposing (..)
import Result
import RemoteData exposing (RemoteData(..), WebData)
import Dict

import Types.Translation exposing (Translation, Translations)
import Types.Exam as Exam exposing (Exam(..), Validity(..))
import Types.Score as Score

-- MODEL

type AppStage = Bootstrap | Game | GameOver

type alias Model =
    { exam : WebData Exam
    , currentAppStage : AppStage
    , userInput : String
    , flash : String
    }

initialModel : Model
initialModel =
    Model NotAsked Bootstrap "" ""

init : ( Model, Cmd Msg )
init =
    ( { initialModel | exam = Loading }
    , loadTranslations
    )

-- UPDATE

type Msg
    = LoadingComplete (Result Http.Error String)
    | ShuffledTranslation (Translations)
    | UserInput String
    | Submit

translationsUrl : String
translationsUrl =
    "https://dl.dropboxusercontent.com/u/4800046/word-list.csv"

loadTranslations : Cmd Msg
loadTranslations =
    translationsUrl
        |> Http.getString
        |> Http.send LoadingComplete

parseCsv : String -> Translations
parseCsv csv =
    let
        trimTrailingComma =
            Regex.replace (AtMost 1) (regex ",$") (\_ -> "")
        splitOnComma =
            String.split ","
        makeTranslation strings =
            case strings of
                englishWord :: frenchWord :: equivalentFrenchWords ->
                    Just (Translation englishWord (Nonempty frenchWord equivalentFrenchWords))
                _ ->
                    Nothing
    in
        csv
            |> String.toLower
            |> String.lines
            |> List.filterMap (trimTrailingComma >> splitOnComma >> makeTranslation)

shuffleTranslations : Translations -> Cmd Msg
shuffleTranslations translations =
    let
        randomGenerator = Random.List.shuffle translations
    in
        Random.generate (ShuffledTranslation) randomGenerator

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadingComplete (Ok csvContent) ->
            ( model, shuffleTranslations <| parseCsv csvContent )
        LoadingComplete (Err error) ->
            ( { model | exam = Failure error }, Cmd.none )
        ShuffledTranslation shuffledTranslations ->
            case Exam.fromTranslations shuffledTranslations of
                Just exam ->
                    ( { model
                          | exam = Success exam
                          , currentAppStage = Game
                      }
                    , Cmd.none
                    )
                Nothing ->
                    let
                        status = { code = 200, message = "Ok" }
                        headers = Dict.empty
                        body = "<CSV file content>"
                        errorMessage = "Could not prepare an exam from the reference file."
                        failure =
                            Http.Response translationsUrl status headers body
                                |> BadPayload errorMessage
                    in
                        ( { model | exam = Failure failure }, Cmd.none )
        UserInput input ->
                ( { model | userInput = input }
                , Cmd.none
                )
        Submit ->
            ( { model
                  | exam = RemoteData.map (Exam.submitAnswer model.userInput) model.exam
                  , userInput = ""
              }
            , Cmd.none
            )

-- VIEW

viewBootstrap : Html Msg
viewBootstrap =
    text "Bootstrapping..."

viewExercise : Model -> Html Msg
viewExercise model =
    let
        toText aTranslation =
            "Please translate \"" ++ aTranslation.englishWord ++ "\""
        question =
            model.exam
                |> RemoteData.map (Exam.mapCurrentExercise toText)
                |> RemoteData.withDefault "I could not prepare a new exercise."
    in
        div []
            [ text question
            , input [ onInput UserInput, value model.userInput ] [ ]
            , button [ onClick Submit ] [ text "Submit"]
            ]

viewFlash : Model -> Html Msg
viewFlash model =
    div []
        [ text model.flash ]

viewScore : Model -> Html Msg
viewScore model =
    let
        score =
            model.exam
                |> RemoteData.map (Exam.score >> Score.toText)
                |> RemoteData.withDefault "0/0"
    in
        div []
            [ text <| "Your score: " ++ score ]

view : Model -> Html Msg
view model =
    case model.currentAppStage of
        Bootstrap ->
            div []
                [ viewFlash model
                , viewBootstrap
                ]
        Game ->
            div []
                [ viewFlash model
                , viewExercise model
                , viewScore model
                ]
        GameOver ->
            div []
                [ text "Game Over!"
                , viewScore model
                ]

-- MAIN

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
