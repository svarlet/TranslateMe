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

import Types.Translation exposing (Translation, Translations)
import Types.Exam as Exam exposing (Exam(..), Validity(..))
import Types.Score as Score

-- MODEL

type View = Bootstrap | Game | GameOver

type alias Model =
    { translations : WebData Translations
    , exam : Maybe Exam
    , currentView : View
    , userInput : String
    , flash : String
    }

initialModel : Model
initialModel =
    Model NotAsked Nothing Bootstrap "" ""

init : ( Model, Cmd Msg )
init =
    ( { initialModel | translations = Loading }
    , loadTranslations
    )

-- UPDATE

type Msg
    = LoadingComplete (Result Http.Error String)
    -- | RandomTranslationPicked (Maybe Translation, Translations)
    | ShuffledTranslation (Translations)
    | UserInput String
    | Submit

loadTranslations : Cmd Msg
loadTranslations =
    "https://dl.dropboxusercontent.com/u/4800046/word-list.csv"
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
            let
                translations = parseCsv csvContent
            in
                ( { model | translations = Success translations }
                , shuffleTranslations translations
                )
        LoadingComplete (Err error) ->
            let
                flashMessage =
                    case error of
                        BadUrl message ->
                            "The URL of the resource file is invalid."
                        Timeout ->
                            "The request for the resource file timed out."
                        NetworkError ->
                            "The request for the resource file could not complete due to a network error."
                        BadStatus _ ->
                            "The request for the resource file returned with a bad status code"
                        BadPayload _ _ ->
                            "The request for the resource file returned with invalid or corrupted data."
            in
                ( { model
                      | translations = Failure error
                      , flash = flashMessage
                  }
                , Cmd.none
                )
        ShuffledTranslation shuffledTranslations ->
            let
                maybeExam = Exam.fromTranslations shuffledTranslations
            in
                case maybeExam of
                    Just exam ->
                        ( { model
                              | currentView = Game
                              , exam = maybeExam
                          }
                        , Cmd.none
                        )
                    Nothing ->
                        ( { model
                              | flash = "Error: I could not prepare an exam from the list of translations."
                          }
                        , Cmd.none
                        )
        UserInput input ->
                ( { model | userInput = input }
                , Cmd.none
                )
        Submit ->
            case model.exam of
                Just exam ->
                    case Exam.submitAnswer exam model.userInput of
                        (Pass, updatedExam) ->
                            ( { model
                                  | flash = "Correct!"
                                  , userInput = ""
                                  , exam = Just updatedExam
                              }
                            , Cmd.none
                            )
                        (Fail, updatedExam) ->
                            ( { model
                                  | flash = "Wrong!"
                                  , userInput = ""
                                  , exam = Just updatedExam
                              }
                            , Cmd.none
                            )
                Nothing ->
                    ( { model | flash = "You can't submit an answer, you are not in an exam!" }
                    , Cmd.none
                    )

-- VIEW

viewBootstrap : Html Msg
viewBootstrap =
    text "Bootstrapping..."

viewTranslationExercise : Model -> Html Msg
viewTranslationExercise model =
    case model.exam of
        Just exam ->
            let
                toQuestion aTranslation =
                    "Please translate " ++ "\"" ++ aTranslation.englishWord ++ "\""
            in
                div []
                    [ text <| Exam.mapCurrentQuestion toQuestion exam
                    , input [ onInput UserInput, value model.userInput ] [ ]
                    , button [ onClick Submit ] [ text "Submit"]
                    ]
        Nothing ->
            text "Sorry, I could not prepare another question for you. :-("

viewFlash : Model -> Html Msg
viewFlash model =
    case model.flash of
        "" ->
            div [] []
        _ ->
            div []
                [ text model.flash ]

viewScore : Model -> Html Msg
viewScore model =
    case model.exam of
        Just exam ->
            let
                score = Exam.score exam
            in
                div []
                    [ text ("Your score: " ++ (Score.toText score) ) ]
        Nothing ->
            text "You have no score because there is no exam in progress."

view : Model -> Html Msg
view model =
    case model.currentView of
        Bootstrap ->
            div []
                [ viewFlash model
                , viewBootstrap
                ]
        Game ->
            div []
                [ viewFlash model
                , viewTranslationExercise model
                , viewScore model
                ]
        GameOver ->
            case model.exam of
                Just exam ->
                    let
                        score = Exam.score exam
                    in
                        div []
                            [ text "Game Over!"
                            , text <| "Your score is" ++ (Score.toText score)
                            ]
                Nothing ->
                    text "Errr, that's weird, there is no exam in progress so you shouldn't be in a game over state..."

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

-- TODO: handle empty list of translations (ie when playing all existing translations)
