module Bootstrap exposing (Model, update, view)

import Html exposing (Html, div, text)
import Http

-- Model

type Status
    = Loading
    | Loaded
    | Failed

type alias Model =
    { rawContent : String
    , status : Status
    }

type Msg
    = Downloaded (Result Http.Error String)
    | Ready

initialModel : Model
initialModel =
    Model "" Loading

init : ( Model, Cmd Msg )
init =
    ( initialModel, downloadWords )

fileUrl : String
fileUrl =
    "https://dl.dropboxusercontent.com/u/4800046/word-list.csv"

downloadWords : Cmd Msg
downloadWords =
    fileUrl
        |> Http.getString
        |> Http.send Downloaded

-- Update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Downloaded (Ok content) ->
            ( {model
                  | rawContent = content
                  , status = Loaded
              }
            , Cmd.none
            )
        Downloaded (Err _) ->
            ( {model | status = Failed}, Cmd.none )
        Ready ->
            ( model, Cmd.none )

-- View

view : Model -> Html Msg
view model =
    case model.status of
        Loading ->
            div
                []
                [ text "Loading..." ]
        Loaded ->
            div
                []
                [ text "Loaded!"]
        Failed ->
            div
                []
                [ text "Failed!" ]
