module Types.Exam exposing (Exam(..), init, passCurrentQuestion, failCurrentQuestion, score)

import Types.Translation exposing (Translation)
import Types.Score as Score exposing (Score)

type Result
    = Pass
    | Fail

type Question =
    Question Translation

type Answer =
    Answer Question Result

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

next : Exam -> Result -> Exam
next (Exam exam) result =
    let
        (q, qxs) =
            case exam.remaining of
                [] ->
                    (exam.current, [])
                question :: rest ->
                    (question, rest)
    in
        Exam
          { answered = exam.answered ++ [ Answer exam.current result ]
          , current = q
          , remaining = qxs
          }

passCurrentQuestion : Exam -> Exam
passCurrentQuestion exam =
    next exam Pass

failCurrentQuestion : Exam -> Exam
failCurrentQuestion exam =
    next exam Fail

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
