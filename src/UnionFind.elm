module UnionFind exposing (UnionFind, connected, count, find, get, quickUnionPathCompression, union)

import Dict exposing (Dict)


type UnionFind id
    = QuickUnionPathCompression Int (Dict id id)


quickUnionPathCompression : UnionFind comparable
quickUnionPathCompression =
    QuickUnionPathCompression 0 Dict.empty


count : UnionFind comparable -> Int
count (QuickUnionPathCompression count_ _) =
    count_


get : comparable -> UnionFind comparable -> comparable
get id (QuickUnionPathCompression _ dict) =
    Maybe.withDefault id (Dict.get id dict)


find : comparable -> UnionFind comparable -> comparable
find id (QuickUnionPathCompression _ dict) =
    findFast id dict


connected : comparable -> comparable -> UnionFind comparable -> Bool
connected left right uf =
    find left uf == find right uf


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
