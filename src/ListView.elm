module ListView exposing (createGraph, viewVars)

import Graph as G exposing (..)
import Html exposing (Html)
import Html.Attributes as HA


createGraph : List Int -> G.Graph Int String
createGraph cbt =
    G.fromNodesAndEdges
        (createNodes cbt)
        (createEdges cbt)


createNodes : List Int -> List (Node Int)
createNodes cbt =
    List.indexedMap (\i v -> G.Node i v) cbt


createEdges : List Int -> List (Edge String)
createEdges cbt =
    let
        n =
            List.length cbt
    in
    List.range 1 (n - 1)
        |> List.indexedMap (\i v -> G.Edge i v "->")


viewVars : List ( String, Int ) -> Html msg
viewVars vars =
    Html.div
        [ HA.style "font-family" "monospace"
        , HA.style "font-size" "1.3em"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "justify-content" "center"
        , HA.style "min-width" "30%"
        ]
        (List.map
            (\( name, val ) ->
                Html.div
                    []
                    [ Html.text <| name ++ " = " ++ String.fromInt val
                    ]
            )
            vars
        )
