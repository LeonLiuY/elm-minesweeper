module Main exposing (..)

import Html exposing (Html, button, div, text, tr, table)
import Html.Attributes  exposing(style)
import Html.App as App
import Cell exposing (CellValue(..), CellStatus(..))
import MapGenerator exposing(..)
import Random exposing(generate)
import Debug
import Set exposing(Set)


main =
    App.program { init = init, view = view, update = update, subscriptions = subscriptions }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- MODEL


type alias ID =
    Int


type alias CellRow =
    List ( ID, Cell.Model )


type alias Model =
    List ( ID, CellRow )


gameSize =
    9

mineCount = 10

init : (Model, Cmd Msg)
init =
    (List.map initRow [1..gameSize], generate (\set -> Start set) <| unique 10 (Random.int 1 100))


initRow : Int -> ( ID, CellRow )
initRow idx =
    ( idx, List.map initCell [1..gameSize] )


initCell : Int -> ( ID, Cell.Model )
initCell idx =
    ( idx, { value = Mine, status = Covered, raised = False } )



-- UPDATE


type Msg
    = Action ID ID Cell.Msg |
      Start (Set Int)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Start list ->
          let one = Debug.log (toString list)  1
          in
          (model, Cmd.none)
        Action row col cellMsg ->
            let
                updateCol ( colID, cell ) =
                    if (colID == col) then
                        ( colID, Cell.update cellMsg cell )
                    else
                        ( colID, cell )

                updateRow ( rowID, cells ) =
                    if rowID == row then
                        ( rowID
                        , (List.map
                            updateCol
                            cells
                          )
                        )
                    else
                        ( rowID, cells )
            in
                (List.map
                    updateRow
                    model, Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
    div [style [("position", "absolute"), ("top", "0"), ("right", "0"), ("bottom", "0"), ("left", "0"), ("background", "#EEEEEE")]]
        [ table
            [style [("margin", "128px auto 0 auto")]]
          <|
            List.map viewCellRow model
        ]


viewCellRow : ( ID, CellRow ) -> Html Msg
viewCellRow ( row, cells ) =
    tr [] <|
        List.map
            (\( col, cell ) ->
                viewCell row col cell
            )
            cells


viewCell : ID -> ID -> Cell.Model -> Html Msg
viewCell row col cell =
    App.map (Action row col) (Cell.view cell)
