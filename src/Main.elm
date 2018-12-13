module Main exposing (..)

import Browser
import Cell exposing (CellStatus(..), CellValue(..), Msg(..))
import Html exposing (Html, button, div, span, table, text, tr)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import List exposing (..)
import MineGenerator exposing (..)
import Platform.Cmd
import Random exposing (generate)
import Set exposing (Set)


main : Program (Maybe Int) Model Msg
main =
    Browser.document { init = init, view = view, update = update, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MODEL


type alias CellRow =
    List Cell.Model


type alias Board = List CellRow

type alias Model =
    { gameSize : Int
    , mineCount : Int
    , board : Board
    }

init : Maybe Int -> ( Model, Cmd Msg )
init flags =
    let gameSize = 9
        mineCount = 9
    in
    ( { gameSize = gameSize, mineCount=  mineCount, board= []}, 
    generate (\mines -> Start mines) <| mines mineCount (Random.pair (Random.int 0 (gameSize - 1)) (Random.int 0 (gameSize - 1))) )



-- UPDATE


type Msg
    = Action Int Int Cell.Msg
    | NewGame
    | Start (Set ( Int, Int ))
    | NoOp Cell.Msg


genMap : Set ( Int, Int ) -> Int -> Board
genMap mines gameSize =
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
                (List.range 0 (gameSize - 1))
        )
        (List.range 0 (gameSize - 1))


clearZero : Board -> Int -> Board
clearZero model gameSize =
    let
        fill =
            { raised = False, status = Covered, value = Number -1 }

        fillRow =
            repeat gameSize fill

        top =
            fillRow :: model

        bottom =
            append (drop 1 model) [ fillRow ]

        ( newModel, moreList ) =
            unzip <|
                map3
                    (\origin top1 bottom1 ->
                        let
                            types =
                                [ fill :: origin
                                , append (drop 1 origin) [ fill ]
                                , top1
                                , fill :: top1
                                , append (drop 1 top1) [ fill ]
                                , bottom1
                                , fill :: bottom1
                                , append (drop 1 bottom1) [ fill ]
                                ]
                        in
                        foldl
                            (\check ( target, more1 ) ->
                                let
                                    ( newRow, moreList1 ) =
                                        unzip <|
                                            map2
                                                (\check1 target1 ->
                                                    if target1.status /= Opened && check1.status == Opened && check1.value == Number 0 then
                                                        ( { target1 | status = Opened }, target1.value == Number 0 )

                                                    else
                                                        ( target1, False )
                                                )
                                                check
                                                target
                                in
                                ( newRow, foldl (||) more1 moreList1 )
                            )
                            ( origin, False )
                            types
                    )
                    model
                    top
                    bottom

        more =
            foldl (||) False moreList
    in
    if more then
        clearZero newModel gameSize

    else
        newModel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start mines ->
            ( {model | board = genMap mines model.gameSize}, Cmd.none )

        Action row col cellMsg ->
            let
                updateCol colID cell =
                    if colID == col then
                        Cell.update cellMsg cell

                    else
                        cell

                updateRow rowID cells =
                    if rowID == row then
                        List.indexedMap
                            updateCol
                            cells

                    else
                        cells

                newBoard =
                    List.indexedMap
                        updateRow
                        model.board
            in
            ( if cellMsg == Open then
                {model | board = clearZero newBoard model.gameSize}

              else
                {model | board = newBoard}
            , Cmd.none
            )

        NewGame ->
            init Nothing

        NoOp cellMsg ->
            ( model, Cmd.none )



-- VIEW


type GameStatus
    = Normal
    | Over
    | Success


status : Board -> GameStatus
status board =
    if
        List.any
            (\row ->
                List.any
                    (\cell ->
                        cell.status == Opened && cell.value == Mine
                    )
                    row
            )
            board
    then
        Over

    else if
        List.all
            (\row ->
                List.all
                    (\cell ->
                        (cell.status == Opened && cell.value /= Mine) || (cell.status == Marked && cell.value == Mine)
                    )
                    row
            )
            board
    then
        Success

    else
        Normal


view : Model -> Browser.Document Msg
view model =
    let
        gameStatus =
            status model.board

        common =
            [ div [ style "margin" "128px 0 24px 0" ]
                [ span [ style "margin-right" "24px" ] [ text <| "Game size: " ++ String.fromInt model.gameSize ++ " x " ++ String.fromInt model.gameSize ]
                , span []
                    [ text <| "Total mines: " ++ String.fromInt model.mineCount
                    , div
                        [ style "margin-left" "24px"
                        , style "display" "inline-block"
                        , style "position" "relative"
                        , style "width" "120px"
                        , style "height" "32px"
                        , style "line-height" "32px"
                        , style "border-radius" "2px"
                        , style "font-size" "0.9em"
                        , style "color" "#fff"
                        , style "background-color" "#4CAF50"
                        , style "box-shadow" "0 2px 5px 0 rgba(0, 0, 0, 0.26)"
                        , style "text-align" "center"
                        , style "cursor" "pointer"
                        , onClick NewGame
                        ]
                        [ text "New Game" ]
                    ]
                ]
            , table
                []
              <|
                List.indexedMap (viewCellRow gameStatus) model.board
            ]
    in
    { title = "Minesweeper"
    , body =
        [ div
            [ style "position" "absolute"
            , style "top" "0"
            , style "right" "0"
            , style "bottom" "0"
            , style "left" "0"
            , style "background" "#EEEEEE"
            , style "display" "flex"
            , style "flex-direction" "column"
            , style "align-items" "center"
            ]
            (case gameStatus of
                Normal ->
                    common

                Success ->
                    append common [ div [ style "color" "#4CAF50", style "margin" "24px 0" ] [ text "You win!" ] ]

                Over ->
                    append common [ div [ style "color" "#E91E63", style "margin" "24px 0" ] [ text "Game over!" ] ]
            )
        ]
    }


viewCellRow : GameStatus -> Int -> CellRow -> Html Msg
viewCellRow gameStatus row cells =
    tr [] <|
        List.indexedMap
            (viewCell gameStatus row)
            cells


viewCell : GameStatus -> Int -> Int -> Cell.Model -> Html Msg
viewCell gameStatus row col cell =
    let
        mapper =
            if gameStatus == Normal then
                Action row col

            else
                NoOp
    in
    Html.map mapper (Cell.view cell)
