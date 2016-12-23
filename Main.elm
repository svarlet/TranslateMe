module Main exposing (..)

import Html exposing (Html, Attribute, div, input, text)
import Platform.Sub exposing (..)
import Game
import Bootstrap

-- MODEL

type alias Model =
    { gameModel : Game.Model
    , bootstrapModel : Bootstrap.Model
    , view : View
    }

initialModel : Model
initialModel =
    Model Game.initialModel Bootstrap.initialModel Bootstrap

init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.map BootstrapMsg Bootstrap.downloadWords )

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
                view =
                    case updatedModel.status of
                        Bootstrap.Loaded ->
                            Game
                        _ ->
                            Bootstrap
            in
                ( { model
                      | bootstrapModel = updatedModel
                      , view = view
                  }
                , Cmd.map BootstrapMsg cmd
                )

-- VIEW

type View
    = Bootstrap
    | Game

view : Model -> Html Msg
view model =
    case model.view of
        Bootstrap ->
            Html.map BootstrapMsg (Bootstrap.view model.bootstrapModel)
        Game ->
            Html.map GameMsg (Game.view model.gameModel)

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
