module Main exposing (..)

import Html exposing (Html, Attribute, div, input, text, button, p, dl, dt, dd, span, em, img)
import Html.Attributes exposing (class, value, placeholder, type_, src, style)
import Html.Events exposing (onInput, onClick, on, keyCode)
import Http exposing (Error(..))
import List.Nonempty exposing (Nonempty(..))
import Platform.Sub exposing (..)
import Random
import Random.List
import Regex exposing (..)
import Result
import RemoteData exposing (RemoteData(..), WebData)
import Dict
import Json.Decode

import Types.Translation exposing (Translation, Translations)
import Types.Exam as Exam exposing (Exam, Exercise(..), Validity(..))
import Types.Score as Score

-- MODEL

type AppStage = Bootstrap | Game | GameOver

type alias Model =
    { exam : WebData Exam
    , currentAppStage : AppStage
    , userInput : String
    }

initialModel : Model
initialModel =
    Model NotAsked Bootstrap ""

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
            let
                twentyTranslations = List.take 20 shuffledTranslations
            in
                case Exam.fromTranslations twentyTranslations of
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
            let
                nextStage isFinished =
                    if isFinished then GameOver else model.currentAppStage
                nextAppStage =
                    model.exam
                        |> RemoteData.map (Exam.isFinished >> nextStage)
                        |> RemoteData.withDefault model.currentAppStage
            in
                ( { model
                      | exam = RemoteData.map (Exam.submitAnswer model.userInput) model.exam
                      , userInput = ""
                      , currentAppStage = nextAppStage
                  }
                , Cmd.none
                )

-- VIEW

viewBootstrap : Html Msg
viewBootstrap =
    text "Bootstrapping..."

onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.Decode.succeed msg
            else
                Json.Decode.fail "not ENTER"
    in
        on "keydown" (Json.Decode.andThen isEnter keyCode)

viewExercise : Model -> Html Msg
viewExercise model =
    let
        extractEnglishWord (Exercise t _) =
            t.englishWord
        englishWord =
            model.exam
                |> RemoteData.map (Exam.mapCurrentExercise extractEnglishWord)
                |> RemoteData.withDefault "ERROR: I could not prepare a new exercise."
    in
        div []
            [ p [ class "lead text-center text-uppercase" ]
                [ text englishWord ]
            , div [ class "input-group" ]
                [ input
                      [ class "form-control"
                      , value model.userInput
                      , type_ "text"
                      , placeholder "Your answer"
                      , onInput UserInput
                      , onEnter Submit
                      ]
                      []
                , span
                      [ class "input-group-btn" ]
                      [ button
                            [ class "btn btn-primary"
                            , onClick Submit ]
                            [ span [ class "glyphicon glyphicon-menu-right" ] [] ]
                      ]
                ]
            ]

viewScore : Model -> Html Msg
viewScore model =
    let
        score =
            model.exam
                |> RemoteData.map (Exam.score >> Score.toText)
                |> RemoteData.withDefault "0/0"
    in
        p [ class "text-center" ]
            [ text <| "Your score: " ++ score ]

viewPreviousResults : Model -> Html Msg
viewPreviousResults model =
    let
        correctAnswersOf =
            .frenchTranslation
            >> List.Nonempty.toList
            >> List.intersperse ", "
            >> String.concat
        validityToCssClass v =
            case v of
                Pass -> class "text-success"
                Fail -> class "text-danger"
                _ -> class "text-warning"
        viewResult (Exercise t v) =
            [ dt []
                  [ text t.englishWord ]
            , dd [ validityToCssClass v ]
                [ em []
                      [ text <| correctAnswersOf t ]
                ]
            ]
        htmlResults =
            model.exam
                |> RemoteData.map (Exam.currentResults >> List.reverse >> List.map viewResult >> List.concat)
                |> RemoteData.withDefault []
    in
        dl [ class "dl-horizontal" ] htmlResults

viewProgress : Model -> Html Msg
viewProgress model =
    let
        (passed, failed, skipped, notAnswered) =
            model.exam
                |> RemoteData.map Exam.status
                |> RemoteData.withDefault (0,0,0,0)
        _ = Debug.log (toString (passed, failed, skipped, notAnswered)) "lol"
        exerciseCount = passed + failed + skipped + notAnswered
        toPercentage a b =
            (toFloat a) / (toFloat b)
                |> (*) 100
                |> ceiling
                |> toString
                |> (flip String.append) "%"
    in
        div [ class "progress" ]
            [ div [ class "progress-bar progress-bar-success"
                  , style [ ("width", toPercentage passed exerciseCount) ]
                  ]
                  []
            , div [ class "progress-bar progress-bar-warning"
                  , style [ ("width", toPercentage skipped exerciseCount) ]
                  ]
                  []
            , div [ class "progress-bar progress-bar-danger"
                  , style [ ("width", toPercentage failed exerciseCount) ]
                  ]
                  []
            ]

view : Model -> Html Msg
view model =
    let
        viewContent =
            case model.currentAppStage of
                Bootstrap ->
                    div [ ]
                        [ viewBootstrap ]
                Game ->
                    div []
                        [ viewExercise model
                        , viewScore model
                        , viewProgress model
                        , viewPreviousResults model
                        ]
                GameOver ->
                    div [ ]
                        [ p [ class "well lead text-center text-uppercase" ] [ text "Game Over!" ]
                        , viewScore model
                        , viewPreviousResults model
                        ]
    in
        div [ class "container" ]
            [ div [ class "col-sm-6 col-sm-offset-3"]
                  [ div [ class "jumbotron" ]
                        [ img [ class "img-responsive", src "logo.png" ] []
                        , viewContent
                        ]
                  ]
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
