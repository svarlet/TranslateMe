module Types.Exam exposing
    ( Exam
    , Exercise(..)
    , Validity(..)
    , mapCurrentExercise
    , passCurrentExercise
    , failCurrentExercise
    , score
    , fromTranslations
    , submitAnswer
    , currentResults
    , isFinished
    , status
    )

import List.Nonempty
import Pivot exposing (Pivot)

import Types.Translation exposing (Translation, Translations)
import Types.Score as Score exposing (Score)

type Validity
    = NotAnswered
    | Skipped
    | Pass
    | Fail

type Exercise =
    Exercise Translation Validity

type alias Exam =
    Pivot Exercise

fromTranslations : Translations -> Maybe Exam
fromTranslations translations =
    translations
        |> List.map (\t -> Exercise t NotAnswered)
        |> Pivot.fromList

next : Exam -> Validity -> Exam
next exam validity =
    let
        validateExercise validity (Exercise translation _) =
            Exercise translation validity
    in
        exam
            |> Pivot.mapC (validateExercise validity)
            |> Pivot.withRollback Pivot.goR

passCurrentExercise : Exam -> Exam
passCurrentExercise exam =
    next exam Pass

failCurrentExercise : Exam -> Exam
failCurrentExercise exam =
    next exam Fail

skipCurrentExercise : Exam -> Exam
skipCurrentExercise exam =
    next exam Skipped

mapCurrentExercise : (Exercise -> a) -> Exam -> a
mapCurrentExercise f exam =
    f <| Pivot.getC exam

submitAnswer : String -> Exam -> Exam
submitAnswer submission exam =
    let
        lowerCaseSubmission =
            String.toLower submission
        containsSubmission =
            String.contains lowerCaseSubmission
        validateSubmissionFor (Exercise translation _) =
            List.Nonempty.any containsSubmission translation.frenchTranslation
    in
        if String.trim submission == "" then
            skipCurrentExercise exam
        else if validateSubmissionFor <| Pivot.getC exam then
            passCurrentExercise exam
        else
            failCurrentExercise exam

score : Exam -> Score
score exam =
    let
        updateScore exercise score =
            case exercise of
                Exercise _ Pass -> Score.succeed score
                _ -> Score.fail score
    in
        (Pivot.getL exam ++ [Pivot.getC exam] ++ Pivot.getR exam)
            |> List.foldl updateScore Score.init

isNotAnswered : Exercise -> Bool
isNotAnswered e =
    case e of
        Exercise _ NotAnswered ->
            True
        _ ->
            False

currentResults : Exam -> List Exercise
currentResults exam =
    if Pivot.hasR exam then
        Pivot.getL exam
    else if isNotAnswered <| Pivot.getC exam then
        Pivot.getL exam
    else
        Pivot.getL exam ++ [Pivot.getC exam]

isFinished : Exam -> Bool
isFinished exam =
    not <| Pivot.hasR exam

status : Exam -> (Int, Int, Int, Int)
status exam =
    let
        updateCounter exercise (passed, failed, skipped, notAnswered) =
            case exercise of
                Exercise _ Pass ->
                    (passed + 1, failed, skipped, notAnswered)
                Exercise _ Fail ->
                    (passed, failed + 1, skipped, notAnswered)
                Exercise _ Skipped ->
                    (passed, failed, skipped + 1, notAnswered)
                Exercise _ NotAnswered ->
                    (passed, failed, skipped, notAnswered + 1)
    in
        (Pivot.getL exam ++ [Pivot.getC exam] ++ Pivot.getR exam)
            |> List.foldl updateCounter (0, 0, 0, 0)
