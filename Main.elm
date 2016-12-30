module Main exposing (..)

import Html exposing (Html, Attribute, div, input, text, button)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput, onClick)
import Http
import List.Nonempty exposing (Nonempty(..))
import Platform.Sub exposing (..)
import Random
import Random.List
import Regex exposing (..)
import Result
import RemoteData exposing (RemoteData(..), WebData)

-- MODEL

type alias Translation =
    { englishWord : String
    , frenchTranslation : Nonempty String
    }

type alias Translations
    = List Translation

type View = Bootstrap | Game | GameOver

type alias Score =
    (Int, Int)

type alias Model =
    { translations : WebData Translations
    , currentTranslation : Maybe Translation
    , currentView : View
    , userInput : String
    , score : Score
    , flash : String
    }

initialModel : Model
initialModel =
    Model NotAsked Nothing Bootstrap "" (0, 0) ""

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

pickRandomTranslation : Translations -> Cmd Msg
pickRandomTranslation translations =
    let
        translationRandomGenerator = Random.List.choose translations
    in
        Random.generate (RandomTranslationPicked) translationRandomGenerator

validateSubmission : String -> Maybe Translation -> Result String Int
validateSubmission userInput maybeTranslation =
    let
        containsUserInput = String.contains userInput
        findSubmissionIn = List.Nonempty.any containsUserInput
    in
        Result.fromMaybe ("You played all existing translations! Game Over :P !") maybeTranslation
            |> Result.map (.frenchTranslation >> findSubmissionIn)
            |> Result.map (\b -> if b == True then 1 else 0)

incrementScore : Score -> Score
incrementScore (x, y) =
    (x + 1, y + 1)

decrementScore : Score -> Score
decrementScore (x, y) =
    (x, y + 1)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadingComplete (Ok csvContent) ->
            let
                translations = parseCsv csvContent
            in
                ( { model
                      | translations = Success translations
                      , currentView = Game
                  }
                , pickRandomTranslation translations
                )
        LoadingComplete (Err error) ->
            ( { model | translations = Failure error }
            , Cmd.none
            )
        RandomTranslationPicked (maybeTranslation, remainingTranslations) ->
            ( { model
                  | translations = Success remainingTranslations
                  , currentTranslation = maybeTranslation
                  , flash = ""
              }
            , Cmd.none
            )
        UserInput input ->
                ( { model | userInput = input }
                , Cmd.none
                )
        Submit ->
            case validateSubmission model.userInput model.currentTranslation of
                Err message ->
                    ( { model | flash = message }
                    , Cmd.none
                    )
                Ok 0 ->
                    ( { model
                          | score = decrementScore model.score
                          , flash = "Wrong answer :("}
                    , Cmd.none
                    )
                Ok bonus ->
                    let
                        isGameOver =
                            case model.translations of
                                Success [] ->
                                    True
                                Success _ ->
                                    False
                                _ ->
                                    True
                        updatedModel =
                            if isGameOver then
                                { model
                                    | score = incrementScore model.score
                                    , currentView = GameOver
                                }
                            else
                                { model
                                    | score = incrementScore model.score
                                    , userInput = ""
                                }
                        nextCommand =
                            RemoteData.map pickRandomTranslation model.translations
                                |> RemoteData.withDefault Cmd.none
                    in
                        ( updatedModel, nextCommand )

-- VIEW

viewBootstrap : Html Msg
viewBootstrap =
    text "Bootstrapping..."

viewTranslationExercise : Model -> Html Msg
viewTranslationExercise model =
    case model.currentTranslation of
        Just aTranslation ->
            div []
                [ text <| "Please translate \"" ++ aTranslation.englishWord ++ "\""
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
    let
        scoreToString (x, y) =
            (toString x) ++ "/" ++ (toString y)
    in
        div []
            [ text ("Your score: " ++ scoreToString model.score) ]

view : Model -> Html Msg
view model =
    case model.currentView of
        Bootstrap ->
            viewBootstrap
        Game ->
            div []
                [ viewFlash model
                , viewTranslationExercise model
                , viewScore model
                ]
        GameOver ->
            div []
                [ text "Game Over!"
                , text <| "Your score is" ++ (toString model.score)
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

-- TODO: handle file download failure
-- TODO: handle empty list of translations (ie when playing all existing translations)
