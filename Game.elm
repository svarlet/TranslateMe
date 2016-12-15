module Game exposing (..)

import List.Extra exposing (getAt)
import Html exposing (Html, div, text)
import Random exposing (generate, int)

-- MODEL

type alias Translation =
    List String

type alias Model =
    { score : Int
    , translations : List Translation
    , translation : Translation
    , input : String
    }

initialModel : Model
initialModel =
    Model 0 [] [] ""

-- UPDATE

type Msg
    = UpdateInput String
    | PickIndex
    | RandomIndexPicked Int

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PickIndex ->
            let
                randomGeneratorUpperBound = List.length model.translations - 1
                positionGenerator = Random.int 0 randomGeneratorUpperBound
            in
                ( model, Random.generate (RandomIndexPicked) positionGenerator )
        UpdateInput newInput ->
            ( { model | input = newInput }, Cmd.none )
        RandomIndexPicked index ->
            case List.Extra.getAt index model.translations of
                Just aTranslation ->
                    ( { model | translation = aTranslation }, Cmd.none )
                Nothing ->
                    ( model, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
    div
        []
        [text "Placeholder from the Game module"]
