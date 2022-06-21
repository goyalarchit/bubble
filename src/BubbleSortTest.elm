port module BubbleSortTest exposing (..)

import DrivingTest as DT exposing (State)
import Html exposing (Html)
import Html.Attributes as HA
import Json.Encode as JE
import List.Extra as LE
import ListView as LV
import Random as R
import Render as R
import Utils exposing (..)


port analytics : JE.Value -> Cmd msg


type alias Model =
    { numbers : List Int
    , i : Int
    , b : Int
    }


type Msg
    = Increment
    | SwapAndIncrement
    | DecrementBAndResetI
    | GotRandom (List Int)


actionToName : Msg -> String
actionToName msg =
    case msg of
        Increment ->
            "Increment i"

        SwapAndIncrement ->
            "Swap And Increment i"

        DecrementBAndResetI ->
            "Decrement b And Reset i"

        GotRandom _ ->
            "Internal"


update : Msg -> State () Model -> State () Model
update msg state =
    let
        ts =
            state.ts

        n =
            List.length state.ts.numbers
    in
    case msg of
        Increment ->
            if ts.i < n - 1 then
                let
                    newTS =
                        { ts | i = ts.i + 1 }
                in
                { state | ts = newTS }

            else
                state

        SwapAndIncrement ->
            if ts.i < n - 1 then
                let
                    nums =
                        LE.swapAt ts.i (ts.i + 1) ts.numbers

                    newTS =
                        { ts | i = ts.i + 1, numbers = nums }
                in
                { state | ts = newTS }

            else
                state

        DecrementBAndResetI ->
            if ts.b > 0 then
                let
                    newTS =
                        { ts | i = 0, b = ts.b - 1 }
                in
                { state | ts = newTS }

            else
                state

        GotRandom a ->
            { state | ts = { i = 0, b = List.length a, numbers = a } }


isEnabled : Msg -> State () Model -> Result String String
isEnabled msg state =
    let
        { i, b, numbers } =
            state.ts

        n =
            List.length state.ts.numbers

        okTxt =
            actionToName msg |> okMessage
    in
    case msg of
        Increment ->
            if i < n - 1 then
                Ok okTxt

            else
                Err (errMessage "i will go out of bounds")

        SwapAndIncrement ->
            if i < n - 1 then
                Ok okTxt

            else
                Err (errMessage "i will go out of bounds")

        DecrementBAndResetI ->
            if b > 0 then
                Ok okTxt

            else
                Err (errMessage "Error! b will go out of bounds")

        GotRandom _ ->
            Ok "You can now begin the test."


next : State () Model -> List Msg
next state =
    let
        { i, b, numbers } =
            state.ts
    in
    if i == 0 && b == 1 then
        []

    else if i < b - 1 then
        case ordered i (i + 1) numbers of
            LT ->
                [ Increment ]

            EQ ->
                [ Increment, SwapAndIncrement ]

            GT ->
                [ SwapAndIncrement ]

    else
        [ DecrementBAndResetI ]


ordered : Int -> Int -> List Int -> Order
ordered i j nums =
    let
        ( ai, aj ) =
            ( LE.getAt i nums, LE.getAt j nums )
    in
    case ( ai, aj ) of
        ( Just a_i, Just a_j ) ->
            if a_i < a_j then
                LT

            else if a_i == a_j then
                EQ

            else
                GT

        _ ->
            GT


view : State () Model -> Html Msg
view state =
    let
        { i, b, numbers } =
            state.ts
    in
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex" "1"
        ]
        [ LV.viewVars [ ( "index (i)", i ), ( "boundary (b)", b ) ]
        , LV.viewNums numbers i b
        ]


btns =
    [ DT.primaryButton (actionToName Increment) Increment
    , DT.primaryButton (actionToName SwapAndIncrement) SwapAndIncrement
    , DT.primaryButton (actionToName DecrementBAndResetI) DecrementBAndResetI
    ]


init : () -> ( State () Model, Cmd Msg )
init _ =
    ( State () (Model [ 5, 4, 3, 2, 1 ] 0 5), R.generate GotRandom (R.list 6 (R.int 1 10)) )


msgType : Msg -> DT.MsgType
msgType msg =
    case msg of
        GotRandom _ ->
            DT.UIMsg

        _ ->
            DT.TSMsg


main =
    DT.sandbox
        { init = init
        , view = view
        , btns = btns
        , update = update
        , isEnabled = isEnabled
        , next = next
        , msgType = msgType
        , analyticsPort = analytics
        }
