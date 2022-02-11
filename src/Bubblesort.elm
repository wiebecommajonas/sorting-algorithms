module Bubblesort exposing (bubblesort)

import Browser exposing (element)
import List.Extra
import Sorter exposing (Log, SortingList)


bubblesortHelper : SortingList -> Int -> Int -> List Log -> List Log
bubblesortHelper list element end log =
    if end <= 1 then
        log

    else if element >= end then
        bubblesortHelper list 1 (end - 1) log

    else
        let
            ( thisId, thisValue ) =
                List.Extra.getAt element list |> Maybe.withDefault ( -1, -1 )

            ( prevId, prevValue ) =
                List.Extra.getAt (element - 1) list |> Maybe.withDefault ( -1, -1 )
        in
        if thisValue < prevValue then
            let
                nextList =
                    List.Extra.swapAt element (element - 1) list
            in
            bubblesortHelper nextList (element + 1) end (log ++ [ { pivot = prevId, element = thisId, result = nextList } ])

        else
            bubblesortHelper list (element + 1) end (log ++ [ { pivot = prevId, element = thisId, result = list } ])


bubblesort : SortingList -> List Log
bubblesort list =
    bubblesortHelper list 1 (List.length list) []
