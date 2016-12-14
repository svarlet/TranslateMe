module Main exposing (..)

import Html exposing (Html, Attribute, div, input, text)
import List.Extra exposing (getAt)
import Platform.Sub exposing (..)

-- MODEL

type alias Translation = List String

type alias Model =
    { score : Int
    , words : List (List String)
    , translation : Translation
    , input : String
    }

init : ( Model, Cmd Msg )
init =
    ( Model 0 [] [] "", Cmd.none )

-- UPDATE

type Msg
    = UpdateInput String
    | RandomIndexPicked Int

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInput newInput ->
            ( { model | input = newInput }, Cmd.none )

        RandomIndexPicked index ->
            case List.Extra.getAt index model.words of
                Just aTranslation ->
                    ( { model | translation = aTranslation }, Cmd.none )
                Nothing ->
                    ( model, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
    div
        []
        [text "Placeholder from the Main module"]

-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
