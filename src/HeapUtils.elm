module HeapUtils exposing (..)

import List.Extra as LE


getForList : List Int -> Int -> Int
getForList lst i =
    case LE.getAt i lst of
        Just x ->
            x

        Nothing ->
            -1


largest : Int -> List Int -> Int
largest i cbt =
    let
        indices =
            [ i, 2 * i + 1, 2 * i + 2 ]

        values =
            List.map (\x -> getForList cbt x) indices
    in
    case List.indexedMap (\t a -> ( t, a )) values |> LE.maximumBy Tuple.second of
        Just ( idx, a ) ->
            getForList indices idx

        Nothing ->
            -1


largestAll : Int -> List Int -> List Int
largestAll i cbt =
    let
        indices =
            [ i, 2 * i + 1, 2 * i + 2 ]

        values =
            List.map (\x -> getForList cbt x) indices
    in
    case List.maximum values of
        Just v ->
            List.indexedMap (\t a -> ( t, a )) values
                |> List.filter (\t -> Tuple.second t == v)
                |> List.map (\t -> getForList indices (Tuple.first t))

        Nothing ->
            []


heapifySubTree : Int -> List Int -> List Int
heapifySubTree i cbt =
    let
        output =
            largest i cbt
    in
    if output /= i then
        heapifySubTree output (LE.swapAt i output cbt)

    else
        cbt


heapifyTree : List Int -> List Int
heapifyTree cbt =
    let
        n =
            List.length cbt
    in
    if n >= 2 then
        List.range 0 (n // 2 - 1)
            |> List.foldr (\i cbt_ -> heapifySubTree i cbt_) cbt

    else
        cbt


isHeap : List Int -> Bool
isHeap cbt =
    heapifyTree cbt == cbt
