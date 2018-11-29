module Cell exposing (CellStatus(..), CellValue(..), Model, Msg(..), update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json

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
    , raised : Bool
    }


type alias Model =
    Cell


type Msg
    = Open
    | ToggleMark
    | Raise
    | Drop
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

        Raise ->
            { model
                | raised = True
            }

        Drop ->
            { model
                | raised = False
            }

        NoOp ->
            model

alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
  ( msg, True )

view : Model -> Html Msg
view model =
    let
        contextmenu msg = preventDefaultOn "contextmenu" (Json.map alwaysPreventDefault (Json.succeed msg))
    in
    case model.status of
        Covered ->
            td [ classList [("covered", True), ("raised", model.raised)], onMouseEnter Raise, onMouseLeave Drop, onClick Open, contextmenu ToggleMark ] []

        Opened ->
            td [ classList [("opened", True)],  contextmenu NoOp]
                [ text <|
                    case model.value of
                        Mine ->
                            "*"

                        Number n ->
                            String.fromInt n
                ]

        Marked ->
            td [ classList [("marked", True), ("raised", model.raised)], onMouseEnter Raise, onMouseLeave Drop, contextmenu ToggleMark ] []
