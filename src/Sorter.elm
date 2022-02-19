module Sorter exposing (..)

import Set exposing (Set)


type Sorter
    = Quicksort
    | Bubblesort
    | Selectionsort


type alias SortingList =
    List ( Int, Float )


type alias SortingFunction =
    SortingList -> List Log


type alias Log =
    { pivot : Int, elements : Set Int, result : SortingList }


allSorters : List Sorter
allSorters =
    [ Quicksort, Bubblesort, Selectionsort ]


nameOf : Sorter -> String
nameOf sorter =
    case sorter of
        Quicksort ->
            "Quicksort"

        Bubblesort ->
            "Bubblesort"

        Selectionsort ->
            "Selectionsort"


toSorter : String -> Sorter
toSorter name =
    case name of
        "Quicksort" ->
            Quicksort

        "Bubblesort" ->
            Bubblesort

        "Selectionsort" ->
            Selectionsort

        _ ->
            Quicksort
