port module Main exposing (..)

import Browser
import Bubblesort exposing (bubblesort)
import Dict exposing (update)
import Html exposing (div, text)
import Html.Attributes exposing (class, href, id, rel, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Dec
import Json.Encode as Enc
import Process
import Quicksort exposing (quicksort)
import Random
import Result exposing (withDefault)
import Selectionsort exposing (selectionsort)
import Set exposing (Set)
import Sorter exposing (Log, Sorter, SortingFunction, SortingList, allSorters, nameOf, toSorter)
import Task


port saveSession : String -> Cmd msg


port loadSession : (String -> msg) -> Sub msg


port doLoadSession : () -> Cmd msg


type Msg
    = Reset
    | NewSample (List Float)
    | ChangeSorter Sorter
    | ChangeSpeed Float
    | LoadSession String
    | Sort
    | ShowLog


sortingFunction : Sorter -> SortingFunction
sortingFunction sorter =
    case sorter of
        Sorter.Quicksort ->
            quicksort

        Sorter.Bubblesort ->
            bubblesort

        Sorter.Selectionsort ->
            selectionsort


type alias Model =
    { sampleSize : Int
    , sample : SortingList
    , speed : Float
    , primary : Set Int
    , secondary : Set Int
    , sorter : Sorter
    , log : List Log
    }


generateSample : Int -> Cmd Msg
generateSample size =
    Random.generate NewSample (Random.list size (Random.float 10 90))


init : () -> ( Model, Cmd Msg )
init _ =
    ( { sampleSize = 0
      , sample = []
      , speed = 10
      , sorter = Sorter.Quicksort
      , primary = Set.empty
      , secondary = Set.empty
      , log = []
      }
    , Cmd.batch [ generateSample 50, doLoadSession () ]
    )


showLog : Float -> Cmd Msg
showLog speed =
    Process.sleep speed
        |> Task.perform (\_ -> ShowLog)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( { model | primary = Set.empty, secondary = Set.empty, log = [] }, generateSample model.sampleSize )

        NewSample sample ->
            ( { model | sampleSize = List.length sample, sample = List.map2 (\a -> \b -> ( a, b )) (List.range 1 (List.length sample)) sample }, Cmd.none )

        ChangeSorter sorter ->
            let
                session =
                    Enc.object [ ( "speed", Enc.float model.speed ), ( "sorter", Enc.string <| nameOf sorter ) ] |> Enc.encode 0
            in
            ( { model | sorter = sorter }, saveSession session )

        ChangeSpeed speed ->
            let
                session =
                    Enc.object [ ( "speed", Enc.float speed ), ( "sorter", Enc.string <| nameOf model.sorter ) ] |> Enc.encode 0
            in
            ( { model | speed = speed }, saveSession session )

        LoadSession value ->
            let
                speed =
                    Dec.decodeString (Dec.field "speed" Dec.float) value |> withDefault 10

                sorter =
                    Dec.decodeString (Dec.field "sorter" Dec.string) value |> withDefault "" |> toSorter
            in
            ( { model | speed = speed, sorter = sorter }, Cmd.none )

        Sort ->
            ( { model | log = sortingFunction model.sorter model.sample }, showLog model.speed )

        ShowLog ->
            case model.log of
                [] ->
                    ( { model | secondary = Set.empty, primary = Set.empty }, Cmd.none )

                l :: ls ->
                    ( { model | sample = l.result, primary = Set.fromList [ l.pivot ], secondary = l.elements, log = ls }, showLog model.speed )


standardAttributes : ( Int, Float ) -> List (Html.Attribute msg)
standardAttributes ( index, value ) =
    [ id <| String.fromInt index, class "sample", style "height" <| String.concat [ String.fromFloat value, "%" ] ]


computeAttributes : Model -> ( Int, Float ) -> List (Html.Attribute msg)
computeAttributes model ( index, value ) =
    if Set.member index model.primary then
        standardAttributes ( index, value ) ++ [ class "highlighted--primary" ]

    else if Set.member index model.secondary then
        standardAttributes ( index, value ) ++ [ class "highlighted--secondary" ]

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
                            [ Html.button [ onClick <| ChangeSorter item ] [ text <| nameOf item ] ]
                    )
                    allSorters
                )
            , Html.ul []
                [ Html.li [] [ Html.input [ type_ "range", Html.Attributes.min "1", Html.Attributes.max "100", value <| String.fromFloat <| 500 / model.speed, onInput <| \input -> ChangeSpeed (String.toFloat input |> Maybe.withDefault 1 |> (/) 500) ] [] ]
                , Html.li [] [ Html.button [ onClick Sort ] [ text "Sort" ] ]
                , Html.li [] [ Html.button [ onClick Reset ] [ text "Reset" ] ]
                ]
            ]
        , Html.div [ class "sample-frame" ]
            [ Html.div [ class "sample__wrapper" ] (List.map (\sample -> div (computeAttributes model sample) []) model.sample) ]
        , Html.div [ class "cc" ]
            [ Html.span [] [ text "by " ], Html.a [ target "_blank", rel "noopener noreferrer", href "https://github.com/wiebecommajonas" ] [ text "wiebecommajonas" ] ]
        ]
    }


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> loadSession LoadSession
        }
