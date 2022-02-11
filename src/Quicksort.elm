module Quicksort exposing (quicksort)

import List.Extra
import Sorter exposing (Log, SortingList)


quicksortHelper : SortingList -> Int -> List ( Int, Int, Int ) -> List Log -> List Log
quicksortHelper list element pivots log =
    case pivots of
        [] ->
            log

        ( pivot, start, end ) :: ps ->
            if element >= end && end - start <= 1 then
                case ps of
                    [] ->
                        log

                    ( nextPivot, _, _ ) :: _ ->
                        quicksortHelper list (nextPivot + 1) ps log

            else if element >= end then
                let
                    newPivots =
                        ps ++ [ ( start, start, pivot ), ( pivot + 1, pivot + 1, end ) ]

                    ( nextPivot, _, _ ) =
                        List.head newPivots |> Maybe.withDefault ( -1, -1, -1 )
                in
                quicksortHelper list (nextPivot + 1) newPivots log

            else
                let
                    ( elId, elValue ) =
                        List.Extra.getAt element list |> Maybe.withDefault ( -1, -1 )

                    ( pivId, pivValue ) =
                        List.Extra.getAt pivot list |> Maybe.withDefault ( -1, -1 )
                in
                if elValue < pivValue then
                    let
                        newList =
                            List.take pivot list ++ ( elId, elValue ) :: List.drop pivot (List.Extra.removeIfIndex ((==) element) list)

                        newLog =
                            List.append log [ { pivot = pivId, element = elId, result = newList } ]
                    in
                    quicksortHelper newList (element + 1) (( pivot + 1, start, end ) :: ps) newLog

                else
                    quicksortHelper list (element + 1) pivots <| List.append log [ { pivot = pivId, element = elId, result = list } ]


quicksort : SortingList -> List Log
quicksort list =
    quicksortHelper list 1 [ ( 0, 0, List.length list ) ] []
