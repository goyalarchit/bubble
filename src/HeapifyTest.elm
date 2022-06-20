port module HeapifyTest exposing (..)

import DrivingTest as DT exposing (State)
import HeapView exposing (createGraph)
import Html as H exposing (Html)
import Json.Encode as JE
import List.Extra as LE
import Random as R
import Render as R
import Render.StandardDrawers as RSD
import Render.StandardDrawers.Attributes as RSDA
import Utils exposing (..)
import Graph as G 
import Tuple3 as T3 
import HeapUtils exposing (..)



port analytics : JE.Value -> Cmd msg


type alias Model =
    { cbt : List Int
    , i : Int
    , p : Int
    }


type Msg
    = SwapAndMoveLeft
    | SwapAndMoveRight
    | ChangeParentAndReset
    | GotRandom (List Int)


update : Msg -> State () Model -> State () Model
update msg state =
    let
        ts =
            state.ts

        n =
            List.length ts.cbt

        leftChild =
            2 * ts.i + 1

        rightChild =
            2 * ts.i + 2
    in
    case msg of
        SwapAndMoveLeft ->
            if leftChild < n then
                let
                    nums =
                        LE.swapAt ts.i leftChild ts.cbt

                    newTS =
                        { ts | i = leftChild, cbt = nums }
                in
                { state | ts = newTS }

            else
                state

        SwapAndMoveRight ->
            if rightChild < n then
                let
                    nums =
                        LE.swapAt ts.i rightChild ts.cbt

                    newTS =
                        { ts | i = rightChild, cbt = nums }
                in
                { state | ts = newTS }

            else
                state

        ChangeParentAndReset ->
            if ts.p > 0 then
                let
                    newTS =
                        { ts | i = ts.p - 1, p = ts.p - 1 }
                in
                { state | ts = newTS }

            else
                state

        GotRandom a ->
            let
                n_ = (List.length a)
            in
            
            { state | ts = { i = n_ // 2 - 1, cbt = a, p = n_ // 2 - 1 } }




next : State () Model -> List Msg
next state =
    let
        { cbt, i, p } =
            state.ts

        largest =
            largestAll i cbt
    in
    if p == 0 && (List.member i largest) then
        []

    else if not (List.member i largest) then
        List.map 
        (\j -> if j == 2*i+1 then SwapAndMoveLeft else SwapAndMoveRight) 
        largest

    else
        [ ChangeParentAndReset ]

xlbl : Model -> (G.Node Int) -> (String,String,Float)
xlbl model n = 
    if (n.id == model.i && n.id == model.p) then ("i,p","4 4",2) 
    else if(n.id == model.i) then ("i","4 4",2) 
    else if(n.id == model.p) then ("p","4 4",2) 
    else ("","",1)



view : State () Model -> Html Msg
view state =
    let
        { cbt, i, p } =
            state.ts
    in
    H.div
        []
        [ R.draw
            []
            [ R.style "height: 100%; width: 100%"
            , R.nodeDrawer (RSD.svgDrawNode 
            [ RSDA.label (\x -> String.fromInt x.label) 
            , RSDA.xLabel (xlbl state.ts  >> T3.first) 
            , RSDA.strokeDashArray (xlbl state.ts  >> T3.second) 
            , RSDA.strokeWidth (xlbl state.ts  >> T3.third)  
            ])
            ]
            (createGraph cbt)
        ]

actionToName : Msg -> String 
actionToName msg = 
    case msg of 
        SwapAndMoveLeft -> "Swap And Move (LeftChild)"
        SwapAndMoveRight -> "Swap And Move (RightChild)"
        ChangeParentAndReset -> "ChangeParent And Reset"
        GotRandom a -> "Internal"

btns =
    [ DT.primaryButton (actionToName SwapAndMoveLeft) SwapAndMoveLeft
    , DT.primaryButton (actionToName SwapAndMoveRight) SwapAndMoveRight
    , DT.primaryButton (actionToName ChangeParentAndReset) ChangeParentAndReset
    ]


init : () -> ( State () Model, Cmd Msg )
init _ =
    ( State () (Model [ 1, 2, 3, 4, 5, 6, 7 ] 0 5), R.generate GotRandom (R.list 7 (R.int 1 10)) )


msgType : Msg -> DT.MsgType
msgType msg =
    case msg of
        GotRandom _ ->
            DT.UIMsg

        _ ->
            DT.TSMsg


isEnabled : Msg -> State () Model -> Result String String
isEnabled msg state =
    let
        { i, p, cbt} =
            state.ts
        n =
            List.length cbt

        leftChild =
            2 * i + 1

        rightChild =
            2 * i + 2

        name = actionToName msg 
    in
    case msg of
        SwapAndMoveLeft -> 
            if leftChild < n then Ok (okMessage name)
            else Err (errMessage "No left child")

        SwapAndMoveRight -> 
            if rightChild < n then Ok (okMessage name)
            else Err (errMessage "No right child")

        ChangeParentAndReset -> 
            if p > 0 then Ok (okMessage name)
            else Err (errMessage "No previous node")

        GotRandom _ ->
            Ok "You can now begin the test."


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
