module Selectionsort exposing (selectionsort)

import Html.Attributes exposing (list)
import List.Extra
import Maybe exposing (withDefault)
import Set
import Sorter exposing (Log, SortingList)


selectionsortHelper : SortingList -> Int -> Int -> Int -> List Log -> List Log
selectionsortHelper list start current min log =
    if start < List.length list then
        let
            ( pivId, pivValue ) =
                List.Extra.getAt start list |> withDefault ( -1, -1 )

            ( elId, elValue ) =
                List.Extra.getAt current list |> withDefault ( -1, -1 )

            ( minId, minValue ) =
                List.Extra.getAt min list |> withDefault ( -1, -1 )
        in
        if current < List.length list then
            let
                newMin =
                    if elValue < minValue then
                        current

                    else
                        min
            in
            selectionsortHelper list start (current + 1) newMin (log ++ [ { pivot = pivId, elements = Set.fromList [ minId, elId ], result = list } ])

        else
            let
                newList =
                    if minValue < pivValue then
                        List.Extra.swapAt start min list

                    else
                        list

                newStart =
                    start + 1

                newCurrent =
                    newStart + 1
            in
            selectionsortHelper newList newStart newCurrent newCurrent (log ++ [ { pivot = pivId, elements = Set.fromList [ minId, elId ], result = newList } ])

    else
        log


selectionsort : SortingList -> List Log
selectionsort list =
    selectionsortHelper list 0 1 1 []
