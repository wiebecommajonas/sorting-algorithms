module Sorter exposing (..)


type Sorter
    = Quicksort
    | Bubblesort


type alias SortingList =
    List ( Int, Float )


type alias SortingFunction =
    SortingList -> List Log


type alias Log =
    { pivot : Int, element : Int, result : SortingList }
