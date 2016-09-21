module MineGenerator exposing (mines)

import Random exposing (..)
import Set exposing (Set)


mines : Int -> Generator comparable -> Generator (Set comparable)
mines size generator =
    let
        helper set remaining =
            if remaining <= 0 then
                map (\_ -> set) bool
            else
                generator
                    `andThen`
                        \val ->
                            let
                                newSet =
                                    Set.insert val set
                            in
                                if Set.size newSet == Set.size set then
                                    helper set remaining
                                else
                                    helper newSet (remaining - 1)
    in
        helper Set.empty size
