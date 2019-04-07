module UnionFind exposing (UnionFind, quickUnionPathCompression, union, get, find, connected, count)

{-| The Union Find data structure.

Inspiration has been gotten from the book
[Algorithms](https://algs4.cs.princeton.edu/15uf) by Robert Sedgewick and Kevin Wayne.

@docs UnionFind, quickUnionPathCompression, union, get, find, connected, count

-}

import Dict exposing (Dict)


{-| Representation of Union Find data structure.
You can create Union Find of any comparable primitives.
-}
type UnionFind id
    = QuickUnionPathCompression Int (Dict id id)


{-| There are a number of easy ways to improve the union-find algorithm.
Ideally, we would like every node to link directly to the root of its tree,
but we do not want to pay the price of changing a large number of links.
We can approach the ideal simply by making all the nodes that we do examine directly link to the root.

The amortized cost per operation for this algorithm is known to be logarithmic.

-}
quickUnionPathCompression : UnionFind comparable
quickUnionPathCompression =
    QuickUnionPathCompression 0 Dict.empty


{-| Returns number of connections.

    """
    amountOfConnections : Int
    amountOfConnections =
        quickUnionPathCompression
            |> union 0 1
            |> union 1 2
            |> union 0 2
            |> count      -- would be equal 2
    """

-}
count : UnionFind comparable -> Int
count (QuickUnionPathCompression count_ _) =
    count_


{-| Given an element, returns the "parent" of the element.

    """
    parentOf0 : Int
    parentOf0 =
        quickUnionPathCompression
            |> union 0 1
            |> get 0      -- would be equal 1

    parentOf2 : Int
    parentOf2 =
        quickUnionPathCompression
            |> union 0 1
            |> get 2      -- would be equal 2
    """

-}
get : comparable -> UnionFind comparable -> comparable
get id (QuickUnionPathCompression _ dict) =
    Maybe.withDefault id (Dict.get id dict)


{-| Given an element, returns the leader identifying the component to which the element belongs.

    """
    leaderOf0 : Int
    leaderOf0 =
        quickUnionPathCompression
            |> union 0 1
            |> union 1 2
            |> find 0     -- would be equal 2
    """

-}
find : comparable -> UnionFind comparable -> comparable
find id (QuickUnionPathCompression _ dict) =
    findFast id dict


{-| Given two elements, returns `True` when both are in the same component.

    """
    elements0and3AreConnected : Bool
    elements0and3AreConnected =
        quickUnionPathCompression
            |> union 0 1
            |> union 1 2
            |> union 3 2
            |> connected 0 3  -- would be equal True

    elements0and3AreNotConnected : Bool
    elements0and3AreNotConnected =
        quickUnionPathCompression
            |> union 0 1
            |> union 1 2
            |> union 3 4
            |> connected 0 3  -- would be equal False
    """

-}
connected : comparable -> comparable -> UnionFind comparable -> Bool
connected left right uf =
    find left uf == find right uf


{-| Add connection between two elements.

    """
    unionFind : UnionFind
    unionFind =
        quickUnionPathCompression
            |> union 0 1  -- creates connection between 0 and 1
            |> union 1 2  -- creates connection between 1 and 2
            |> union 0 2  -- already connected
    """

-}
union : comparable -> comparable -> UnionFind comparable -> UnionFind comparable
union left right (QuickUnionPathCompression count_ dict) =
    let
        ( leftRoot, leftDict ) =
            findCompressed left dict

        ( rightRoot, rightDict ) =
            findCompressed right leftDict
    in
    if leftRoot == rightRoot then
        QuickUnionPathCompression count_ rightDict

    else
        QuickUnionPathCompression (count_ + 1) (Dict.insert leftRoot rightRoot rightDict)



-- I N T E R N A L   H E L P E R S


findFast : comparable -> Dict comparable comparable -> comparable
findFast id dict =
    case Dict.get id dict of
        Nothing ->
            id

        Just cursor ->
            if id == cursor then
                id

            else
                findFast cursor dict


findCompressed : comparable -> Dict comparable comparable -> ( comparable, Dict comparable comparable )
findCompressed id dict =
    case Dict.get id dict of
        Nothing ->
            ( id, Dict.insert id id dict )

        Just cursor ->
            if id == cursor then
                ( id, dict )

            else
                let
                    ( parent, nextDict ) =
                        findCompressed cursor dict
                in
                ( parent, Dict.insert id parent nextDict )

