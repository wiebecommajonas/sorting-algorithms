module Main exposing (..)

import Browser
import Dict exposing (update)
import Html exposing (div, text)
import Html.Attributes exposing (class, href, id, style, type_)
import Html.Events exposing (onClick, onInput)
import List.Extra
import Process
import Random
import Set exposing (Set)
import Task


type Msg
    = Reset
    | NewSample (List Float)
    | ChangeSorter Sorter
    | ChangeSpeed Float
    | Sort
    | ShowLog


type Sorter
    = Quicksort


type alias SortingList =
    List ( Int, Float )


type alias SortingFunction =
    SortingList -> List Log


type alias Log =
    { pivot : Int, element : Int, result : SortingList }


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


nameOf : Sorter -> String
nameOf sorter =
    case sorter of
        Quicksort ->
            "Quicksort"


sortingFunction : Sorter -> SortingFunction
sortingFunction sorter =
    case sorter of
        Quicksort ->
            quicksort


type alias Model =
    { sampleSize : Int
    , sample : SortingList
    , speed : Float
    , green : Set Int
    , yellow : Set Int
    , sorter : Sorter
    , log : List Log
    }


generateSample : Int -> Cmd Msg
generateSample size =
    Random.generate NewSample (Random.list size (Random.float 10 90))


allSorters : List Sorter
allSorters =
    [ Quicksort ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( { sampleSize = 0
      , sample = []
      , speed = 10
      , sorter = Quicksort
      , green = Set.empty
      , yellow = Set.empty
      , log = []
      }
    , generateSample 100
    )


showLog : Float -> Cmd Msg
showLog speed =
    Process.sleep speed
        |> Task.perform (\_ -> ShowLog)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( { model | green = Set.empty, yellow = Set.empty, log = [] }, generateSample model.sampleSize )

        NewSample sample ->
            ( { model | sampleSize = List.length sample, sample = List.map2 (\a -> \b -> ( a, b )) (List.range 1 (List.length sample)) sample }, Cmd.none )

        ChangeSorter sorter ->
            ( { model | sorter = sorter }, Cmd.none )

        ChangeSpeed speed ->
            ( { model | speed = speed }, Cmd.none )

        Sort ->
            ( { model | log = sortingFunction model.sorter model.sample }, showLog model.speed )

        ShowLog ->
            case model.log of
                [] ->
                    ( { model | yellow = Set.empty, green = Set.empty }, Cmd.none )

                l :: ls ->
                    ( { model | sample = l.result, green = Set.fromList [ l.pivot ], yellow = Set.fromList [ l.element ], log = ls }, showLog model.speed )


standardAttributes : ( Int, Float ) -> List (Html.Attribute msg)
standardAttributes ( index, value ) =
    [ id <| String.fromInt index, class "sample", style "height" <| String.concat [ String.fromFloat value, "%" ] ]


computeAttributes : Model -> ( Int, Float ) -> List (Html.Attribute msg)
computeAttributes model ( index, value ) =
    if Set.member index model.green then
        standardAttributes ( index, value ) ++ [ class "green" ]

    else if Set.member index model.yellow then
        standardAttributes ( index, value ) ++ [ class "yellow" ]

    else
        standardAttributes ( index, value )


view : Model -> Browser.Document Msg
view model =
    { title = "Sorting algorithms"
    , body =
        [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "../styles/main.css" ] []
        , Html.nav []
            [ Html.ul []
                (List.map
                    (\item ->
                        Html.li
                            (if item == model.sorter then
                                [ class "selected" ]

                             else
                                []
                            )
                            [ Html.a [ href "#", onClick <| ChangeSorter item ] [ text <| nameOf item ] ]
                    )
                    allSorters
                )
            , Html.ul []
                [ Html.li [] [ Html.input [ type_ "range", Html.Attributes.min "5", Html.Attributes.max "200", onInput <| \input -> ChangeSpeed (String.toFloat input |> Maybe.withDefault 10) ] [] ]
                , Html.li [] [ Html.a [ href "#", onClick Sort ] [ text "Sort" ] ]
                , Html.li [] [ Html.a [ href "#", onClick Reset ] [ text "Reset" ] ]
                ]
            ]
        , Html.div [ class "sample-frame" ]
            [ Html.div [ class "sample__wrapper" ] (List.map (\sample -> div (computeAttributes model sample) []) model.sample)
            ]
        ]
    }


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
