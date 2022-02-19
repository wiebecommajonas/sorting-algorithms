module Insertionsort exposing (insertionsort)

import Browser exposing (element)
import Debug exposing (log)
import List.Extra
import Maybe exposing (withDefault)
import Set
import Sorter exposing (Log, SortingList)


insertionsortHelper : SortingList -> Int -> Int -> List Log -> List Log
insertionsortHelper list pivot element log =
    if element < List.length list then
        let
            ( elId, elValue ) =
                List.Extra.getAt element list |> withDefault ( -1, -1 )
        in
        if pivot >= 0 then
            let
                ( pivId, pivValue ) =
                    List.Extra.getAt pivot list |> withDefault ( -1, -1 )
            in
            if pivValue < elValue then
                let
                    newList =
                        List.take (pivot + 1) list ++ ( elId, elValue ) :: List.drop (pivot + 1) (List.Extra.removeAt element list)
                in
                insertionsortHelper newList element (element + 1) (log ++ [ { pivot = elId, elements = Set.fromList [ pivId ], result = newList } ])

            else
                insertionsortHelper list (pivot - 1) element (log ++ [ { pivot = elId, elements = Set.fromList [ pivId ], result = list } ])

        else
            let
                newList =
                    ( elId, elValue ) :: List.Extra.removeAt element list
            in
            insertionsortHelper newList element (element + 1) log

    else
        log


insertionsort : SortingList -> List Log
insertionsort list =
    insertionsortHelper list 0 1 []
