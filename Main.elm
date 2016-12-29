module Main exposing (..)

import Html exposing (Html, Attribute, div, input, text)
import Http
import Platform.Sub exposing (..)
import CsvParser
import Random
import Random.List
import Maybe

-- MODEL

type RemoteData e a
    = NotRequested
    | Loading
    | Success a
    | Failure e

type alias WebData a =
    RemoteData Http.Error a

type alias Translation
    = List String

type alias Translations
    = List Translation

type SelectedTranslation
    = NoSelectionYet
    | Selection (Maybe Translation)

type alias Model =
    { translations : WebData Translations
    , currentTranslation : SelectedTranslation
    }

initialModel : Model
initialModel =
    Model NotRequested NoSelectionYet

init : ( Model, Cmd Msg )
init =
    ( { initialModel | translations = Loading }
    , loadTranslations
    )

-- UPDATE

type Msg
    = LoadingComplete (Result Http.Error String)
    | RandomTranslationPicked (Maybe (List String), Translations)

loadTranslations : Cmd Msg
loadTranslations =
    "https://dl.dropboxusercontent.com/u/4800046/word-list.csv"
        |> Http.getString
        |> Http.send LoadingComplete

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
                translations = CsvParser.parseCsv csvContent
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

-- VIEW

viewBootstrap : Html Msg
viewBootstrap =
    text "Bootstrapping..."

viewTranslationExercise : Maybe Translation -> Html Msg
viewTranslationExercise translation =
    case translation of
        Just aTranslation ->
            let
                englishWord = List.head aTranslation
                frenchWords = List.tail aTranslation
            in
                div []
                    [text <| (toString englishWord) ++ (toString frenchWords)]
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
