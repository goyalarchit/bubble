module HeapView exposing (createGraph)

import Graph as G exposing (..)



-- import Render as R
-- import Render.StandardDrawers as RSD
-- import Render.StandardDrawers.Attributes as RSDA
-- import Html


getValidEdges : Int -> Int -> List (G.Edge String)
getValidEdges i n =
    List.map (\x -> G.Edge i x "->") ([ 2 * i + 1, 2 * i + 2 ] |> List.filter (\idx -> idx < n))



-- createGraph : List Int -> Int -> G.Graph Int String
-- createGraph cbt i =
--     if i < List.length cbt then
--         let
--             leftGraph =
--                 createGraph cbt (2 * i + 1)
--             rightGraph =
--                 createGraph cbt (2 * i + 2)
--         in
--         G.fromNodesAndEdges
--             (G.Node i (getForList cbt i) :: List.append (G.nodes leftGraph) (G.nodes rightGraph))
--             (List.append (getValidEdges i (List.length cbt)) (G.edges leftGraph) |> List.append (G.edges rightGraph))
--     else
--         G.empty


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
    List.range 0 (n - 1)
        |> List.foldl (\i l -> List.append (getValidEdges i n) l) []



-- main : Html.Html msg
-- main =
--     R.draw
--         []
--         [ R.style "height: 100vh;"
--         , R.nodeDrawer (RSD.svgDrawNode [ RSDA.label (\x -> String.fromInt x.label) ])
--         ]
--         (createGraph [ 10, 15, 1, 2, 7, 99, 45, 0 ] )
