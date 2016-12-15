module Main exposing (..)

import Html exposing (Html, Attribute, div, input, text)
import List.Extra exposing (getAt)
import Platform.Sub exposing (..)

-- MODEL

init : ( Model, Cmd Msg )
init =
    ( , Cmd.none )

-- UPDATE

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =

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
