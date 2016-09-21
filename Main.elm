module Main exposing (..)

import Html exposing (Html, button, div, text, tr, table)
import Html.Attributes exposing (style)
import Html.App as App
import Cell exposing (CellValue(..), CellStatus(..))
import MineGenerator exposing (..)
import Random exposing (generate)
import Debug
import Set exposing (Set)


main =
    App.program { init = init, view = view, update = update, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MODEL


type alias CellRow =
    List Cell.Model


type alias Model =
    List CellRow


gameSize =
    9


mineCount =
    10


init : ( Model, Cmd Msg )
init =
    ( [], generate (\mines -> Start mines) <| mines mineCount (Random.pair (Random.int 0 (gameSize - 1)) (Random.int 0 (gameSize - 1))) )



-- UPDATE


type Msg
    = Action Int Int Cell.Msg
    | Start (Set ( Int, Int ))


genMap : Set ( Int, Int ) -> Model
genMap mines =
    List.map
        (\row ->
            List.map
                (\col ->
                    if Set.member ( row, col ) mines then
                        { raised = False, status = Covered, value = Mine }
                    else
                        { raised = False, status = Covered, value = Mine }
                )
                [0..gameSize - 1]
        )
        [0..gameSize - 1]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start mines ->
            let
                one =
                    Debug.log (toString mines) 1
            in
                ( genMap mines, Cmd.none )

        Action row col cellMsg ->
            let
                updateCol colID cell =
                    if (colID == col) then
                        Cell.update cellMsg cell
                    else
                        cell

                updateRow rowID cells =
                    if rowID == row then
                        (List.indexedMap
                            updateCol
                            cells
                        )
                    else
                        cells
            in
                ( List.indexedMap
                    updateRow
                    model
                , Cmd.none
                )



-- VIEW


view : Model -> Html Msg
view model =
    div [ style [ ( "position", "absolute" ), ( "top", "0" ), ( "right", "0" ), ( "bottom", "0" ), ( "left", "0" ), ( "background", "#EEEEEE" ) ] ]
        [ table
            [ style [ ( "margin", "128px auto 0 auto" ) ] ]
          <|
            List.indexedMap viewCellRow model
        ]


viewCellRow : Int -> CellRow -> Html Msg
viewCellRow row cells =
    tr [] <|
        List.indexedMap
            (viewCell row)
            cells


viewCell : Int -> Int -> Cell.Model -> Html Msg
viewCell row col cell =
    App.map (Action row col) (Cell.view cell)
