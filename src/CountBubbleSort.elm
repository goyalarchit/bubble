module CountBubbleSort exposing (..)

import Browser
import DrivingTest exposing (State, generateCorrectRuns)
import HeapSortTest as HT
import HeapUtils as HU
import Html exposing (Html)
import List.Extra as LE
import Random as R


type alias Model =
    { arrays : List ( Int, List Int )
    }


type Msg
    = GotRandom (List Int)


generateList =
    R.int 4 5 |> R.andThen (\l -> R.list l (R.int 1 20))


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [], R.generate GotRandom generateList )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRandom lst ->
            let
                correctSeq =
                    let
                        n_ =
                            List.length lst
                    in
                    generateCorrectRuns HT.next
                        HT.msgType
                        HT.update
                        ( State () { i = 0, cbt = HU.heapifyTree lst, b = n_ - 1 }, [] )

                minLen =
                    List.map (\l -> ( List.length l, l )) correctSeq
                        |> LE.minimumBy Tuple.first
                        |> Maybe.withDefault ( 0, [] )

                newMod =
                    if Tuple.first minLen == 8 then
                        { model | arrays = ( Tuple.first minLen, lst ) :: model.arrays }

                    else
                        model
            in
            if List.length model.arrays < 10 then
                ( newMod, R.generate GotRandom generateList )

            else
                ( newMod, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div
        []
        (List.map
            (\( n, lst ) -> Html.div [] [ Html.text <| (String.join ", " (List.map String.fromInt lst) ++ "   :    " ++ String.fromInt (List.length lst)) ++ "   :    " ++ String.fromInt n ])
            model.arrays
        )


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
