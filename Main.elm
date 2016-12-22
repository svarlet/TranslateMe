module Main exposing (..)

import Html exposing (Html, Attribute, div, input, text)
import Platform.Sub exposing (..)
import Game
import Bootstrap

-- MODEL

type alias Model =
    { gameModel : Game.Model
    , bootstrapModel : Bootstrap.Model
    }

initialModel : Model
initialModel =
    Model Game.initialModel Bootstrap.initialModel

init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )

-- UPDATE

type Msg
    = GameMsg Game.Msg
    | BootstrapMsg Bootstrap.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameMsg gameMsg ->
            let
                (updatedModel, cmd) =
                    Game.update gameMsg model.gameModel
            in
                ( { model | gameModel = updatedModel }, Cmd.map GameMsg cmd )
        BootstrapMsg bootstrapMsg ->
            let
                (updatedModel, cmd) =
                    Bootstrap.update bootstrapMsg model.bootstrapModel
            in
                ( { model | bootstrapModel = updatedModel }, Cmd.map BootstrapMsg cmd )

-- VIEW

view : Model -> Html Msg
view model =
    div
        []
        [text "Placeholder from the Main module"]

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
