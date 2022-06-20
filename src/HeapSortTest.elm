port module HeapSortTest exposing (..)


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
import HeapUtils exposing (..)
import Graph as G 
import Tuple3 as T3 


port analytics : JE.Value -> Cmd msg


type alias Model =
    { cbt : List Int
    , i : Int
    , b : Int
    }


type Msg
    = SwapAndMoveLeft
    | SwapAndMoveRight
    | ResetI
    | SwapIBAndDecrB
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
        
        SwapIBAndDecrB ->
            
            if ts.b > 0 then
                let
                    nums = LE.swapAt ts.i ts.b ts.cbt
                    newTS = { ts | cbt = nums, b = ts.b - 1}
                in
                    { state | ts = newTS}
            else 
                state

            

        ResetI -> 
            let
                newTS = { ts | i = 0 }
            in
                { state | ts = newTS }

        GotRandom a ->
            let
                n_ = (List.length a)
            in
            
            { state | ts = { i = 0, cbt = heapifyTree a, b = n_ - 1} }

actionToName : Msg -> String 
actionToName msg = 
    case msg of 
        SwapAndMoveLeft -> "Swap And Move(LeftChild)"
        SwapAndMoveRight -> "Swap And Move(RightChild)"
        SwapIBAndDecrB -> "Swap i,b And Move(PreviousNode)"
        ResetI -> "Reset i"
        GotRandom a -> "Internal"




isEnabled : Msg -> State () Model -> Result String String
isEnabled msg state =
    let
        { i, b, cbt} =
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

        SwapIBAndDecrB -> 
            if b > 0 then Ok (okMessage name)
            else Err (errMessage "Cannot decrement boundary")

        ResetI -> 
            Ok (okMessage name)

        GotRandom _ ->
            Ok "You can now begin the test."
        
        




next : State () Model -> List Msg
next state =
    let
        { cbt, i, b } =
            state.ts

        largest =
            largestAll i (List.take (b+1) cbt)
    in
    if b == 0 then []
    else if i == 0 && (isHeap (List.take (b+1) cbt)) then
        [SwapIBAndDecrB]
    

    else if not (List.member i largest) then
        List.map 
        (\j -> if j == 2*i+1 then SwapAndMoveLeft else SwapAndMoveRight) 
        largest

    else
        [ ResetI ]

xlbl : Model -> (G.Node Int) -> (String,String,Float)
xlbl model n = 
    if (n.id == model.i && n.id == model.b) then ("i,b","4 4",2) 
    else if(n.id == model.i) then ("i","4 4",2) 
    else if(n.id == model.b) then ("b","4 4",2) 
    else ("","",1)

edgeView : Model -> (G.Edge String) -> Float 
edgeView model e = 
    if e.from > model.b || e.to > model.b then 0 
    else 3



view : State () Model -> Html Msg
view state =
    let
        { cbt, i, b } =
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
            , R.edgeDrawer (RSD.svgDrawEdge [RSDA.strokeWidth (edgeView state.ts)])
            ]
            (createGraph cbt)
        ]


btns =
    [ DT.primaryButton (actionToName SwapAndMoveLeft) SwapAndMoveLeft
    , DT.primaryButton (actionToName SwapAndMoveRight) SwapAndMoveRight
    , DT.primaryButton (actionToName SwapIBAndDecrB) SwapIBAndDecrB
    , DT.primaryButton (actionToName ResetI) ResetI
    ]


init : () -> ( State () Model, Cmd Msg )
init _ =
    ( State () (Model [ 10,9,9,3,5,2,8 ] 0 6), R.generate GotRandom (R.list 7 (R.int 1 10)) )


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
