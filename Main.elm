module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.App as App
import Cell exposing (CellValue(..), CellStatus(..))


main =
    App.beginnerProgram { model = init, view = view, update = update }



-- MODEL


type alias ID =
    Int


type alias Model =
    { cells : List ( ID, Cell.Model )
    , nextID : ID
    }


init : Model
init =
    { cells =
        [ ( 0, { value = Mine, status = Marked } )
        , ( 1, { value = Mine, status = Covered } )
        , ( 2, { value = Mine, status = Opened } )
        , ( 3, { value = Mine, status = Marked } )
        ]
    , nextID = 4
    }



-- UPDATE


type Msg
    = Action ID Cell.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        Action id cellMsg ->
            let
                updateCell ( cellID, cellModel ) =
                    if cellID == id then
                        ( cellID, Cell.update cellMsg cellModel )
                    else
                        ( cellID, cellModel )
            in
                { model | cells = List.map updateCell model.cells }



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
    <|
        List.map viewCell model.cells


viewCell : ( ID, Cell.Model ) -> Html Msg
viewCell ( id, model ) =
    App.map (Action id) (Cell.view model)
