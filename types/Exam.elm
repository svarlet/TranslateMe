module Types.Exam exposing
    ( Exam(..)
    , Validity(..)
    , init
    , passCurrentQuestion
    , failCurrentQuestion
    , score
    , fromTranslations
    , mapCurrentQuestion
    , submitAnswer)

import List.Nonempty

import Types.Translation exposing (Translation, Translations)
import Types.Score as Score exposing (Score)

type Validity
    = Pass
    | Fail

type Question =
    Question Translation

type Answer =
    Answer Question Validity

type Exam =
    Exam
      { answered : List Answer
      , current : Question
      , remaining : List Question
      }


init : Question -> List Question -> Exam
init q qs =
    Exam
      { answered = []
      , current = q
      , remaining = qs
      }

fromTranslations : Translations -> Maybe Exam
fromTranslations translations =
    case translations of
        [] ->
            Nothing
        x :: xs ->
            Just <| init (Question x) <| List.map (Question) xs

mapCurrentQuestion : (Translation -> a) -> Exam -> a
mapCurrentQuestion f (Exam exam ) =
    let
        (Question translation) = exam.current
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
    in
        Exam
          { answered = exam.answered ++ [ Answer exam.current validity ]
          , current = q
          , remaining = qxs
          }

passCurrentQuestion : Exam -> Exam
passCurrentQuestion exam =
    next exam Pass

failCurrentQuestion : Exam -> Exam
failCurrentQuestion exam =
    next exam Fail

submitAnswer : Exam -> String -> (Validity, Exam)
submitAnswer (Exam exam) submission =
    let
        containsSubmission = String.contains submission
        validateSubmissionFor (Question translation) = List.Nonempty.any containsSubmission translation.frenchTranslation
    in
        if validateSubmissionFor exam.current then
            (Pass, passCurrentQuestion (Exam exam))
        else
            (Fail, failCurrentQuestion (Exam exam))

score : Exam -> Score
score (Exam exam) =
    let
        updateScore answer score =
            case answer of
                Answer _ Pass -> Score.succeed score
                Answer _ Fail -> Score.fail score
    in
        exam.answered
            |> List.foldl updateScore Score.init
