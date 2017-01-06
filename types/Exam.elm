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
    )

import List.Nonempty
import Pivot exposing (Pivot)

import Types.Translation exposing (Translation, Translations)
import Types.Score as Score exposing (Score)

type Validity
    = NotAnswered
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

mapCurrentExercise : (Exercise -> a) -> Exam -> a
mapCurrentExercise f exam =
    f <| Pivot.getC exam

passCurrentExercise : Exam -> Exam
passCurrentExercise exam =
    next exam Pass

failCurrentExercise : Exam -> Exam
failCurrentExercise exam =
    next exam Fail

submitAnswer : String -> Exam -> Exam
submitAnswer submission exam =
    let
        containsSubmission =
            String.contains submission
        validateSubmissionFor (Exercise translation _) =
            List.Nonempty.any containsSubmission translation.frenchTranslation
    in
        if validateSubmissionFor <| Pivot.getC exam then
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

currentResults : Exam -> List Exercise
currentResults exam =
    if Pivot.hasR exam then
        Pivot.getL exam
    else
       Pivot.getL exam ++ [Pivot.getC exam]

isFinished : Exam -> Bool
isFinished exam =
    not <| Pivot.hasR exam
