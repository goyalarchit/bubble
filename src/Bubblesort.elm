port module Bubblesort exposing (..)

import DrivingTest as DT exposing (State)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Encode as JE
import List.Extra as LE
import Random as R


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


update : Msg -> State () Model -> State () Model
update msg state =
    case msg of
        Increment ->
            let
                ts =
                    state.ts

                newTS =
                    { ts | i = ts.i + 1 }
            in
            { state | ts = newTS }

        SwapAndIncrement ->
            let
                ts =
                    state.ts

                nums =
                    LE.swapAt ts.i (ts.i + 1) ts.numbers

                newTS =
                    { ts | i = ts.i + 1, numbers = nums }
            in
            { state | ts = newTS }

        DecrementBAndResetI ->
            let
                ts =
                    state.ts

                newTS =
                    { ts | i = 0, b = ts.b - 1 }
            in
            { state | ts = newTS }

        GotRandom a ->
            { state | ts = { i = 0, b = List.length a, numbers = a } }


isEnabled : Msg -> State () Model -> Result String String
isEnabled msg state =
    let
        { i, b, numbers } =
            state.ts
    in
    case msg of
        Increment ->
            let
                incrementable =
                    i < b - 1
            in
            if incrementable && ordered i (i + 1) numbers then
                Ok "Incremented i"

            else
                Err "Cannot increment i"

        SwapAndIncrement ->
            let
                incrementable =
                    i < b - 1
            in
            if incrementable && not (ordered i (i + 1) numbers) then
                Ok "Swapped i and i+1, Incremented i"

            else
                Err "Cannot swap and increment i"

        DecrementBAndResetI ->
            let
                resetable =
                    i == b - 1 && i > 0
            in
            if resetable then
                Ok "Decremented B and resetted i"

            else
                Err "Cannot decrement b and reset i"

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
        if ordered i (i + 1) numbers then
            [ Increment ]

        else
            [ SwapAndIncrement ]

    else
        [ DecrementBAndResetI ]


ordered : Int -> Int -> List Int -> Bool
ordered i j nums =
    let
        ( ai, aj ) =
            ( LE.getAt i nums, LE.getAt j nums )
    in
    case ( ai, aj ) of
        ( Just a_i, Just a_j ) ->
            a_i <= a_j

        _ ->
            False


view : State () Model -> Html Msg
view state =
    let
        { i, b, numbers } =
            state.ts
    in
    H.div
        []
        [ H.div [] [ H.text (List.map String.fromInt numbers |> String.join ", ") ]
        , H.div [] [ H.text ("i : " ++ String.fromInt i) ]
        , H.div [] [ H.text ("b : " ++ String.fromInt b) ]
        , H.div
            []
            [ H.button [ HE.onClick Increment ] [ H.text "Increment I" ]
            , H.button [ HE.onClick SwapAndIncrement ] [ H.text "Swap And Increment I" ]
            , H.button [ HE.onClick DecrementBAndResetI ] [ H.text "Decrement I and Reset B" ]
            ]
        ]


btns =
    [ DT.primaryButton "Increment I" Increment
    , DT.primaryButton "Swap and Increment I" SwapAndIncrement
    , DT.primaryButton "Decrement And Reset" DecrementBAndResetI
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
