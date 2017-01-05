module Types.Exam exposing
    ( Exam(..)
    , Validity(..)
    , init
    , passCurrentExercise
    , failCurrentExercise
    , score
    , fromTranslations
    , mapCurrentExercise
    , submitAnswer)

import List.Nonempty

import Types.Translation exposing (Translation, Translations)
import Types.Score as Score exposing (Score)

type Validity
    = NotAnswered
    | Pass
    | Fail

type Exercise =
    Exercise Translation Validity

type Exam =
    Exam
      { previous : List Exercise
      , current : Exercise
      , remaining : List Exercise
      }

init : Exercise -> List Exercise -> Exam
init e exs =
    Exam
      { previous = []
      , current = e
      , remaining = exs
      }

fromTranslations : Translations -> Maybe Exam
fromTranslations translations =
    let
        toExerciseList =
            List.map (\e -> Exercise e NotAnswered)
    in
        case translations of
            [] ->
                Nothing
            x :: xs ->
                Just <| init (Exercise x NotAnswered) <| toExerciseList xs

mapCurrentExercise : (Translation -> a) -> Exam -> a
mapCurrentExercise f (Exam exam) =
    let
        (Exercise translation _) = exam.current
    in
        f translation

next : Exam -> Validity -> Exam
next (Exam exam) validity =
    let
        (q, qxs) =
            case exam.remaining of
                [] ->
                    (exam.current, [])
                question :: rest ->
                    (question, rest)
        (Exercise t _) = exam.current
    in
        Exam
          { previous = exam.previous ++ [ Exercise t validity ]
          , current = q
          , remaining = qxs
          }

passCurrentExercise : Exam -> Exam
passCurrentExercise exam =
    next exam Pass

failCurrentExercise : Exam -> Exam
failCurrentExercise exam =
    next exam Fail

submitAnswer : String -> Exam -> Exam
submitAnswer submission (Exam exam) =
    let
        containsSubmission =
            String.contains submission
        validateSubmissionFor (Exercise translation _) =
            List.Nonempty.any containsSubmission translation.frenchTranslation
    in
        if validateSubmissionFor exam.current then
            passCurrentExercise (Exam exam)
        else
            failCurrentExercise (Exam exam)

score : Exam -> Score
score (Exam exam) =
    let
        updateScore answer score =
            case answer of
                Exercise _ Pass -> Score.succeed score
                Exercise _ Fail -> Score.fail score
                Exercise _ NotAnswered -> Score.fail score
    in
        exam.previous
            |> List.foldl updateScore Score.init
