port module HeapifyTest exposing (..)

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
    [ [ 4, 15, 14, 1, 13, 8, 5, 17, 2, 15, 6, 3, 12, 4 ]
    , [ 18, 3, 4, 12, 20, 4, 13, 6, 20, 3, 12, 4, 18 ]
    , [ 18, 9, 14, 20, 9, 14, 9, 1, 10, 7, 5, 17, 11, 12, 7 ]
    , [ 1, 14, 7, 10, 2, 6, 16, 18, 17, 3 ]
    , [ 7, 19, 14, 19, 1, 17, 18, 20, 11, 20, 14, 17, 9 ]
    , [ 12, 10, 20, 8, 1, 15, 1, 19, 9, 8, 16, 3, 18 ]
    , [ 15, 16, 18, 10, 12, 5, 5, 16, 20, 17, 14, 2, 2, 8, 3 ]
    , [ 15, 12, 12, 2, 4, 7, 20, 6, 6, 1, 13, 4, 6, 15 ]
    , [ 17, 14, 12, 4, 14, 13, 9, 16, 17, 19, 20, 12, 12 ]
    , [ 12, 11, 3, 2, 12, 9, 6, 20, 6, 4, 4, 11, 15, 3 ]
    ]


initCmd =
    RL.choose arrays
        |> R.andThen
            (\l -> R.constant (Maybe.withDefault [ 12, 11, 3, 2, 12, 9, 6, 20, 6, 4, 4 ] (Tuple.first l)))


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
                n_ =
                    List.length a
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
    if p == 0 && List.member i largest then
        []

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
        [ ChangeParentAndReset ]


xlbl : Model -> G.Node Int -> ( String, String, Float )
xlbl model n =
    if n.id == model.i && n.id == model.p then
        ( "i,p", "4 4", 2 )

    else if n.id == model.i then
        ( "i", "4 4", 2 )

    else if n.id == model.p then
        ( "p", "4 4", 2 )

    else
        ( "", "", 1 )


view : State () Model -> Html Msg
view state =
    let
        { cbt, i, p } =
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
            ]
            (createGraph cbt)
        , H.div
            [ HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            , HA.style "align-items" "center"
            , HA.style "width" "50%"
            , HA.style "justify-content" "space-evenly"
            ]
            [ LV.viewVars [ ( "index (i)", i ), ( "index (p)", p ) ]
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
                , RSDA.title (\x -> String.fromInt x.label)
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
                        [ RSDA.label (varLabel ( model.i, model.p ))
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
varLabel ( i, p ) node =
    case ( node.id == i, node.id == p ) of
        ( True, True ) ->
            "i,p"

        ( True, False ) ->
            "i"

        ( False, True ) ->
            "p"

        ( False, False ) ->
            ""


actionToName : Msg -> String
actionToName msg =
    case msg of
        SwapAndMoveLeft ->
            "Swap And Move (LeftChild)"

        SwapAndMoveRight ->
            "Swap And Move (RightChild)"

        ChangeParentAndReset ->
            "Move (PreviousNode)"

        GotRandom a ->
            "Internal"


btns =
    [ DT.primaryButton (actionToName SwapAndMoveLeft) SwapAndMoveLeft
    , DT.primaryButton (actionToName SwapAndMoveRight) SwapAndMoveRight
    , DT.primaryButton (actionToName ChangeParentAndReset) ChangeParentAndReset
    ]


init : () -> ( State () Model, Cmd Msg )
init _ =
    ( State () (Model [ 1, 2, 3, 4, 5, 6, 7 ] 0 5), R.generate GotRandom initCmd )


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
        { i, p, cbt } =
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

        ChangeParentAndReset ->
            if p > 0 then
                Ok (okMessage name)

            else
                Err (errMessage "No previous node")

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
