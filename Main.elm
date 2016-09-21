module Main exposing (..)

import Html exposing (Html, button, div, text, tr, table)
import Html.Attributes exposing (style)
import Html.App as App
import Cell exposing (CellValue(..), CellStatus(..), Msg(..))
import MineGenerator exposing (..)
import Random exposing (generate)
import Set exposing (Set)
import Platform.Cmd
import List exposing (..)


main : Program Never
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


gameSize : number
gameSize =
    9


mineCount : number
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
                        { raised = False
                        , status = Covered
                        , value =
                            Number <|
                                Set.size <|
                                    Set.intersect mines <|
                                        Set.fromList
                                            [ ( row - 1, col - 1 )
                                            , ( row, col - 1 )
                                            , ( row + 1, col - 1 )
                                            , ( row - 1, col )
                                            , ( row + 1, col )
                                            , ( row - 1, col + 1 )
                                            , ( row, col + 1 )
                                            , ( row + 1, col + 1 )
                                            ]
                        }
                )
                [0..gameSize - 1]
        )
        [0..gameSize - 1]


clearZero : Model -> Model
clearZero model =
    let
        fill =
            { raised = False, status = Covered, value = Number -1 }

        fillRow =
            repeat gameSize fill

        top =
            fillRow :: model

        bottom =
            drop 1 model `append` [ fillRow ]

        ( newModel, moreList ) =
            unzip <|
                map3
                    (\origin top bottom ->
                        let
                            types =
                                [ (fill :: origin)
                                , (drop 1 origin `append` [ fill ])
                                , top
                                , (fill :: top)
                                , (drop 1 top `append` [ fill ])
                                , bottom
                                , (fill :: bottom)
                                , (drop 1 bottom `append` [ fill ])
                                ]

                            ( newRow, more ) =
                                foldl
                                    (\check ( target, more ) ->
                                        let
                                            ( newRow, moreList ) =
                                                unzip <|
                                                    map2
                                                        (\check target ->
                                                            if target.status /= Opened && check.status == Opened && check.value == Number 0 then
                                                                ( { target | status = Opened }, target.value == Number 0 )
                                                            else
                                                                ( target, False )
                                                        )
                                                        check
                                                        target
                                        in
                                            ( newRow, foldl (||) more moreList )
                                    )
                                    ( origin, False )
                                    types
                        in
                            ( newRow, more )
                    )
                    model
                    top
                    bottom

        more =
            foldl (||) False moreList
    in
        if more then
            clearZero newModel
        else
            newModel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start mines ->
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

                newModel =
                    List.indexedMap
                        updateRow
                        model
            in
                ( if cellMsg == Open then
                    clearZero newModel
                  else
                    newModel
                , (Cmd.none)
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
