module Cell exposing (Msg, Model, update, view, CellValue(..), CellStatus(..))

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (succeed)


type CellValue
    = Mine
    | Number Int


type CellStatus
    = Covered
    | Opened
    | Marked


type alias Cell =
    { value : CellValue
    , status : CellStatus
    }


type alias Model =
    Cell


type Msg
    = Open
    | ToggleMark
    | NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        Open ->
            { model | status = Opened }

        ToggleMark ->
            { model
                | status =
                    if model.status == Marked then
                        Covered
                    else
                        Marked
            }

        NoOp ->
            model


styles =
    let
        size =
            "24px"

        common' =
            [ ( "width", size ), ( "height", size ) ]
    in
        { common = common'
        , marked = ( "background", "red" ) :: common'
        }


view : Model -> Html Msg
view model =
    case model.status of
        Covered ->
            td [ style styles.common, onClick Open, (onWithOptions "contextmenu" { defaultOptions | preventDefault = True } (succeed ToggleMark)) ] []

        Opened ->
            td [ style styles.common, (onWithOptions "contextmenu" { defaultOptions | preventDefault = True } (succeed NoOp)) ] []

        Marked ->
            td [ style styles.marked, (onWithOptions "contextmenu" { defaultOptions | preventDefault = True } (succeed ToggleMark)) ] []
