module MineGenerator exposing (mines)

import Random exposing (..)
import Set exposing (Set)

bool: Generator Bool
bool = 
    map (\i -> if i < 1 then True else False) <| int 0 1

mines : Int -> Generator comparable -> Generator (Set comparable)
mines size generator =
    let
        helper set remaining =
            if remaining <= 0 then
                map (\_ -> set) bool

            else
                generator
                    |> andThen
                        (\val ->
                            let
                                newSet =
                                    Set.insert val set
                            in
                            if Set.size newSet == Set.size set then
                                helper set remaining

                            else
                                helper newSet (remaining - 1)
                        )
    in
    helper Set.empty size
