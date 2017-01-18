module Tests exposing (..)

import Expect
import Fuzz exposing (list, int, tuple, string, Fuzzer)
import List.Nonempty exposing (Nonempty(..))
import Maybe
import Pivot
import Test exposing (..)
import Types.Exam as Exam exposing (Exam)
import Types.Score as Score exposing (Score)
import Types.Translation exposing (Translation, Translations)


all : Test
all =
    describe "Sample Test Suite"
        [ scoreTests
        , examTests
        ]


scoreTests : Test
scoreTests =
    describe "Tests for the Score data type"
        [ test "init returns a score of 0/0" <|
            \() ->
                Expect.equal ( 0, 0 ) Score.init
        , fuzz scoreFuzzer "succeed increments both numbers" <|
            \( x, y ) ->
                Expect.equal ( x + 1, y + 1 ) <| Score.succeed ( x, y )
        , fuzz scoreFuzzer "fail increments the second number only" <|
            \( x, y ) ->
                Expect.equal ( x, y + 1 ) <| Score.fail ( x, y )
        , fuzz scoreFuzzer "toText converts both numbers to string and inserts a slash in between" <|
            \( x, y ) ->
                let
                    expected =
                        (toString x) ++ "/" ++ (toString y)

                    actual =
                        Score.toText ( x, y )
                in
                    Expect.equal expected actual
        ]


scoreFuzzer : Fuzzer Score
scoreFuzzer =
    tuple ( int, int )


examTests : Test
examTests =
    describe "Tests for the Exam data type"
        [ test "fromTranslations returns Nothing if the provided translation list is empty" <|
            \() ->
                Expect.equal Nothing <| Exam.fromTranslations []
        , fuzz (list translationFuzzer) "fromTranslations returns an Exam with no previous exercise" <|
            \translations ->
                Expect.equal [] (Exam.fromTranslations translations |> Maybe.map (Pivot.getL) |> Maybe.withDefault [])
        ]


translationFuzzer : Fuzzer Translation
translationFuzzer =
    let
        frenchTranslationFuzzer =
            Fuzz.map2 Nonempty string (list string)

        englishWordFuzzer =
            string
    in
        Fuzz.map2 Translation englishWordFuzzer frenchTranslationFuzzer


newExamFuzzer : Fuzzer (Maybe Exam)
newExamFuzzer =
    Fuzz.map Exam.fromTranslations (list translationFuzzer)
