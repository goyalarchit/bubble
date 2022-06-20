module ListView exposing (createGraph)
-- import Render as R
-- import Render.StandardDrawers as RSD
-- import Render.StandardDrawers.Attributes as RSDA
-- import Html
import Graph as G exposing (..)


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
        n = List.length cbt
    in
        List.range 1 (n - 1)
        |> List.indexedMap (\i v -> (G.Edge i v "->"))

-- main : Html.Html msg
-- main =
--     R.draw
--         []
--         [ R.style "height: 100vh;"
--         , R.nodeDrawer (RSD.svgDrawNode [ RSDA.label (\x -> String.fromInt x.label) ])
--         ]
--         (createGraph [ 10, 15, 1, 2, 7, 99, 45, 0 ] )

