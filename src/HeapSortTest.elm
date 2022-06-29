port module HeapSortTest exposing (..)

import Dagre.Attributes as DA
import DrivingTest as DT exposing (State)
import Graph as G
import HeapUtils exposing (..)
import HeapView exposing (createGraph)
import Html as H exposing (Html)
import Html.Attributes as HA
import Json.Encode as JE
import List.Extra as LE
import ListView as LV
import Random as R
import Random.List as RL
import Render as R
import Render.StandardDrawers as RSD
import Render.StandardDrawers.Attributes as RSDA
import Render.StandardDrawers.Types as RSDT
import Tuple3 as T3
import Utils exposing (..)


port analytics : JE.Value -> Cmd msg


arrays =
    [ [ 9, 2, 2, 19, 4, 14, 14 ]
    , [ 9, 18, 12, 2, 6, 12, 14 ]
    , [ 4, 4, 16, 16, 7, 16, 17 ]
    , [ 18, 6, 4, 11, 6, 8, 6 ]
    , [ 19, 20, 15, 5, 7, 15, 10 ]
    , [ 10, 19, 7, 3, 10, 20, 13 ]
    , [ 8, 4, 19, 9, 4, 15, 15 ]
    , [ 6, 4, 11, 19, 5, 13, 13 ]
    , [ 4, 15, 18, 15, 20, 8, 16 ]
    , [ 20, 2, 8, 16, 8, 9, 8 ]
    ]


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


initCmd =
    RL.choose arrays
        |> R.andThen
            (\l -> R.constant (Maybe.withDefault [ 20, 5, 16, 2, 11 ] (Tuple.first l)))


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
                    nums =
                        LE.swapAt ts.i ts.b ts.cbt

                    newTS =
                        { ts | cbt = nums, b = ts.b - 1 }
                in
                { state | ts = newTS }

            else
                state

        ResetI ->
            let
                newTS =
                    { ts | i = 0 }
            in
            { state | ts = newTS }

        GotRandom a ->
            let
                n_ =
                    List.length a
            in
            { state | ts = { i = 0, cbt = heapifyTree a, b = n_ - 1 } }


actionToName : Msg -> String
actionToName msg =
    case msg of
        SwapAndMoveLeft ->
            "Swap And Move(LeftChild)"

        SwapAndMoveRight ->
            "Swap And Move(RightChild)"

        SwapIBAndDecrB ->
            "Swap i,b And Move(PreviousNode)"

        ResetI ->
            "Reset i"

        GotRandom a ->
            "Internal"


isEnabled : Msg -> State () Model -> Result String String
isEnabled msg state =
    let
        { i, b, cbt } =
            state.ts

        n =
            List.length cbt

        leftChild =
            2 * i + 1

        rightChild =
            2 * i + 2

        name =
            actionToName msg
    in
    case msg of
        SwapAndMoveLeft ->
            if leftChild < n then
                Ok (okMessage name)

            else
                Err (errMessage "No left child")

        SwapAndMoveRight ->
            if rightChild < n then
                Ok (okMessage name)

            else
                Err (errMessage "No right child")

        SwapIBAndDecrB ->
            if b > 0 then
                Ok (okMessage name)

            else
                Err (errMessage "Cannot decrement boundary")

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
            largestAll i (List.take (b + 1) cbt)
    in
    if b == 0 then
        []

    else if i == 0 && isHeap (List.take (b + 1) cbt) then
        [ SwapIBAndDecrB ]

    else if not (List.member i largest) then
        List.map
            (\j ->
                if j == 2 * i + 1 then
                    SwapAndMoveLeft

                else
                    SwapAndMoveRight
            )
            largest

    else
        [ ResetI ]


xlbl : Model -> G.Node Int -> ( String, String, Float )
xlbl model n =
    if n.id == model.i && n.id == model.b then
        ( "i,b", "4 4", 2 )

    else if n.id == model.i then
        ( "i", "4 4", 2 )

    else if n.id == model.b then
        ( "b", "4 4", 2 )

    else
        ( "", "", 1 )


edgeView : Model -> G.Edge String -> Float
edgeView model e =
    if e.from > model.b || e.to > model.b then
        0

    else
        3


view : State () Model -> Html Msg
view state =
    let
        { cbt, i, b } =
            state.ts
    in
    H.div
        [ HA.style "width" "100%"
        , HA.style "height" "40vh"
        , HA.style "display" "flex"
        ]
        [ R.draw
            [ DA.width 24
            , DA.height 24
            , DA.rankSep 30
            , DA.nodeSep 30
            ]
            [ R.style " width: 50%; height: 100%"
            , R.nodeDrawer
                (RSD.svgDrawNode
                    [ RSDA.label (\x -> String.fromInt x.label)
                    , RSDA.xLabels
                        [ RSD.svgDrawXLabel
                            [ RSDA.label (xlbl state.ts >> T3.first)
                            , RSDA.fontSize 16
                            ]
                        ]
                    , RSDA.strokeDashArray (xlbl state.ts >> T3.second)
                    , RSDA.strokeWidth (xlbl state.ts >> T3.third)
                    ]
                )
            , R.edgeDrawer (RSD.svgDrawEdge [ RSDA.strokeWidth (edgeView state.ts) ])
            ]
            (createGraph cbt)
        , H.div
            [ HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            , HA.style "align-items" "center"
            , HA.style "width" "50%"
            , HA.style "justify-content" "space-evenly"
            ]
            [ LV.viewVars [ ( "index (i)", i ), ( "index (b)", b ) ]
            , viewList state.ts
            ]
        ]



-- View List


viewList : Model -> Html msg
viewList model =
    R.draw
        [ DA.rankDir DA.LR
        , DA.marginY 40
        , DA.rankSep 20
        ]
        [ R.style "width: 100%;"
        , R.edgeDrawer
            (RSD.svgDrawEdge
                [ RSDA.strokeWidth (\_ -> 0)
                ]
            )
        , R.nodeDrawer
            (RSD.svgDrawNode
                [ RSDA.label (\x -> String.fromInt x.label)
                , RSDA.strokeWidth (changeShape model.i >> T3.second)
                , RSDA.strokeDashArray (changeShape model.i >> T3.third)
                , RSDA.xLabels
                    [ RSD.svgDrawXLabel
                        [ RSDA.label (\n -> String.fromInt n.id)
                        , RSDA.pos (\_ w h -> ( 0, h ))
                        , RSDA.shape (changeShapeXlabel model.i >> T3.first)
                        , RSDA.strokeWidth (changeShapeXlabel model.i >> T3.second)
                        , RSDA.strokeDashArray (changeShapeXlabel model.i >> T3.third)
                        , RSDA.fontSize 10
                        , RSDA.title (\n -> String.fromInt (n.id + 1))
                        ]
                    , RSD.svgDrawXLabel
                        [ RSDA.label (varLabel ( model.i, model.b ))
                        , RSDA.pos (\_ w h -> ( 0, -4 * h / 5 ))
                        , RSDA.fontSize 14
                        ]
                    ]
                ]
            )
        ]
        (LV.createGraph model.cbt)


changeShape : Int -> G.Node n -> ( RSDT.Shape, Float, String )
changeShape idx node =
    if node.id == idx || node.id == 2 * idx + 1 || node.id == 2 * idx + 2 then
        ( RSDT.Circle, 2, "4" )

    else
        ( RSDT.NoShape, 1, "" )


changeShapeXlabel : Int -> G.Node n -> ( RSDT.Shape, Float, String )
changeShapeXlabel idx node =
    if node.id == idx || node.id == 2 * idx + 1 || node.id == 2 * idx + 2 then
        ( RSDT.Circle, 1, "4" )

    else
        ( RSDT.NoShape, 0, "" )


varLabel : ( Int, Int ) -> G.Node n -> String
varLabel ( i, b ) node =
    case ( node.id == i, node.id == b ) of
        ( True, True ) ->
            "i,b"

        ( True, False ) ->
            "i"

        ( False, True ) ->
            "b"

        ( False, False ) ->
            ""


btns =
    [ DT.primaryButton (actionToName SwapAndMoveLeft) SwapAndMoveLeft
    , DT.primaryButton (actionToName SwapAndMoveRight) SwapAndMoveRight
    , DT.primaryButton (actionToName SwapIBAndDecrB) SwapIBAndDecrB
    , DT.primaryButton (actionToName ResetI) ResetI
    ]


init : () -> ( State () Model, Cmd Msg )
init _ =
    ( State () (Model [ 10, 9, 9, 3, 5, 2, 8 ] 0 6), R.generate GotRandom initCmd )


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
