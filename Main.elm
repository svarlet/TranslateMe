module Main exposing (..)

import Html exposing (Html, Attribute, div, input, text)
import Html.Events exposing (onInput)
import Http
import List.Nonempty exposing (Nonempty(..))
import Maybe
import Platform.Sub exposing (..)
import Random
import Random.List
import Regex exposing (..)

-- MODEL

type RemoteData e a
    = NotRequested
    | Loading
    | Success a
    | Failure e

type alias WebData a =
    RemoteData Http.Error a

type alias Translation =
    { englishWord : String
    , frenchTranslation : Nonempty String
    }

type alias Translations
    = List Translation

type SelectedTranslation
    = NoSelectionYet
    | Selection (Maybe Translation)

type alias Model =
    { translations : WebData Translations
    , currentTranslation : SelectedTranslation
    , userInput : Maybe String
    }

initialModel : Model
initialModel =
    Model NotRequested NoSelectionYet Nothing

init : ( Model, Cmd Msg )
init =
    ( { initialModel | translations = Loading }
    , loadTranslations
    )

-- UPDATE

type Msg
    = LoadingComplete (Result Http.Error String)
    | RandomTranslationPicked (Maybe Translation, Translations)
    | UserInput String

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
            |> String.lines
            |> List.filterMap (trimTrailingComma >> splitOnComma >> makeTranslation)

pickRandomTranslation : Translations -> Cmd Msg
pickRandomTranslation translations =
    let
        translationRandomGenerator = Random.List.choose translations
    in
        Random.generate (RandomTranslationPicked) translationRandomGenerator

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadingComplete (Ok csvContent) ->
            let
                translations = parseCsv csvContent
            in
                ( { model | translations = Success translations }
                , pickRandomTranslation translations
                )
        LoadingComplete (Err error) ->
            ( { model | translations = Failure error }
            , Cmd.none
            )
        RandomTranslationPicked (maybeTranslation, remainingTranslations) ->
            ( { model
                  | translations = Success remainingTranslations
                  , currentTranslation = Selection maybeTranslation
              }
            , Cmd.none
            )
        UserInput input ->
                ( { model | userInput = Just input }
                , Cmd.none
                )

-- VIEW

viewBootstrap : Html Msg
viewBootstrap =
    text "Bootstrapping..."

viewTranslationExercise : Maybe Translation -> Html Msg
viewTranslationExercise translation =
    case translation of
        Just aTranslation ->
            div []
                [ text <| "Please translate \"" ++ aTranslation.englishWord ++ "\""
                , input [ onInput UserInput ] [ text "Your answer here" ]
                , text (toString aTranslation)
                ]
        Nothing ->
            text "Sorry, I could not prepare another question for you. :-("

view : Model -> Html Msg
view model =
    case model.currentTranslation of
        NoSelectionYet ->
            viewBootstrap
        Selection maybeTranslation ->
            viewTranslationExercise maybeTranslation

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

-- TODO: handle file download failure
-- TODO: handle empty list of translations (ie when playing all existing translations)
