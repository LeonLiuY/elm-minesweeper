module MapGenerator exposing (sample, unique)

import Random exposing (..)
import Set exposing (Set)

sample : Int -> Int -> Int -> Generator (List Int)

sample size min max =
  list size (int min max)

unique : Int -> Generator comparable -> Generator (Set comparable)

unique size generator =
  if size == 1 then
  Random.map (\a ->
    Set.singleton a
  ) generator
  else
    Random.map2 (
    \a set->
        Set.insert a set
    ) generator (unique (size - 1) generator)
