module Tests exposing (quickUnionPathCompression)

import Expect exposing (Expectation)
import Test exposing (Test, describe, test)
import UnionFind exposing (UnionFind)


quickUnionPathCompression : Test
quickUnionPathCompression =
    describe "UnionFind.quickUnionPathCompression"
        [ test "reflexive" <|
            \() ->
                UnionFind.quickUnionPathCompression
                    |> UnionFind.connected 0 0
                    |> Expect.true "0 and 0 should be connected"
        , test "symmetric" <|
            \() ->
                UnionFind.quickUnionPathCompression
                    |> UnionFind.union 0 1
                    |> UnionFind.union 1 2
                    |> Expect.all
                        [ UnionFind.connected 0 1 >> Expect.true "0 and 1 should be connected"
                        , UnionFind.connected 1 0 >> Expect.true "1 and 0 should be connected"
                        ]
        , test "transitive" <|
            \() ->
                UnionFind.quickUnionPathCompression
                    |> UnionFind.union 0 1
                    |> UnionFind.union 1 2
                    |> UnionFind.union 0 2
                    |> Expect.all
                        [ UnionFind.connected 0 1 >> Expect.true "0 and 1 should be connected"
                        , UnionFind.connected 1 2 >> Expect.true "1 and 2 should be connected"
                        , UnionFind.connected 0 2 >> Expect.true "0 and 2 should be connected"
                        ]
        , test "not connected" <|
            \() ->
                UnionFind.quickUnionPathCompression
                    |> UnionFind.union 0 1
                    |> UnionFind.union 1 2
                    |> UnionFind.union 0 2
                    |> UnionFind.union 3 4
                    |> Expect.all
                        [ UnionFind.connected 0 3 >> Expect.false "0 and 3 shouldn't be connected"
                        , UnionFind.connected 1 3 >> Expect.false "1 and 3 shouldn't be connected"
                        , UnionFind.connected 2 3 >> Expect.false "2 and 3 shouldn't be connected"
                        ]
        , describe "UnionFind.count"
            [ test "empty" <|
                \() ->
                    UnionFind.quickUnionPathCompression
                        |> UnionFind.count
                        |> Expect.equal 0
            , test "one" <|
                \() ->
                    UnionFind.quickUnionPathCompression
                        |> UnionFind.union 0 1
                        |> UnionFind.count
                        |> Expect.equal 1
            , test "two" <|
                \() ->
                    UnionFind.quickUnionPathCompression
                        |> UnionFind.union 0 1
                        |> UnionFind.union 1 2
                        |> UnionFind.count
                        |> Expect.equal 2
            , test "don't count connected" <|
                \() ->
                    UnionFind.quickUnionPathCompression
                        |> UnionFind.union 0 1
                        |> UnionFind.union 1 2
                        |> UnionFind.union 0 2
                        |> UnionFind.count
                        |> Expect.equal 2
            , test "different components" <|
                \() ->
                    UnionFind.quickUnionPathCompression
                        |> UnionFind.union 0 1
                        |> UnionFind.union 1 2
                        |> UnionFind.union 3 4
                        |> UnionFind.count
                        |> Expect.equal 3
            ]
        , describe "UnionFind.find"
            [ test "leader of initial level" <|
                \() ->
                    UnionFind.quickUnionPathCompression
                        |> UnionFind.find 0
                        |> Expect.equal 0
            , test "leader first level" <|
                \() ->
                    UnionFind.quickUnionPathCompression
                        |> UnionFind.union 0 1
                        |> Expect.all
                            [ UnionFind.find 0 >> Expect.equal 1
                            , UnionFind.find 1 >> Expect.equal 1
                            ]
            , test "leader second level" <|
                \() ->
                    UnionFind.quickUnionPathCompression
                        |> UnionFind.union 0 1
                        |> UnionFind.union 1 2
                        |> Expect.all
                            [ UnionFind.find 0 >> Expect.equal 2
                            , UnionFind.find 1 >> Expect.equal 2
                            , UnionFind.find 2 >> Expect.equal 2
                            ]
            , test "leader third level" <|
                \() ->
                    UnionFind.quickUnionPathCompression
                        |> UnionFind.union 0 1
                        |> UnionFind.union 1 2
                        |> UnionFind.union 2 3
                        |> Expect.all
                            [ UnionFind.find 0 >> Expect.equal 3
                            , UnionFind.find 1 >> Expect.equal 3
                            , UnionFind.find 2 >> Expect.equal 3
                            , UnionFind.find 3 >> Expect.equal 3
                            ]
            , test "leader fourth compressed level" <|
                \() ->
                    UnionFind.quickUnionPathCompression
                        |> UnionFind.union 0 1
                        |> UnionFind.union 1 2
                        |> UnionFind.union 2 3
                        |> UnionFind.union 0 4
                        |> Expect.all
                            [ UnionFind.find 0 >> Expect.equal 4
                            , UnionFind.find 1 >> Expect.equal 4
                            , UnionFind.find 2 >> Expect.equal 4
                            , UnionFind.find 3 >> Expect.equal 4
                            , UnionFind.find 4 >> Expect.equal 4
                            ]
            ]
        , describe "UnionFind.get"
            [ test "parent of initial level" <|
                \() ->
                    UnionFind.quickUnionPathCompression
                        |> UnionFind.get 0
                        |> Expect.equal 0
            , test "parent first level" <|
                \() ->
                    UnionFind.quickUnionPathCompression
                        |> UnionFind.union 0 1
                        |> Expect.all
                            [ UnionFind.get 0 >> Expect.equal 1
                            , UnionFind.get 1 >> Expect.equal 1
                            ]
            , test "parent second level" <|
                \() ->
                    UnionFind.quickUnionPathCompression
                        |> UnionFind.union 0 1
                        |> UnionFind.union 1 2
                        |> Expect.all
                            [ UnionFind.get 0 >> Expect.equal 1
                            , UnionFind.get 1 >> Expect.equal 2
                            , UnionFind.get 2 >> Expect.equal 2
                            ]
            , test "parent third level" <|
                \() ->
                    UnionFind.quickUnionPathCompression
                        |> UnionFind.union 0 1
                        |> UnionFind.union 1 2
                        |> UnionFind.union 2 3
                        |> Expect.all
                            [ UnionFind.get 0 >> Expect.equal 1
                            , UnionFind.get 1 >> Expect.equal 2
                            , UnionFind.get 2 >> Expect.equal 3
                            , UnionFind.get 3 >> Expect.equal 3
                            ]
            , test "parent fourth compressed level" <|
                \() ->
                    UnionFind.quickUnionPathCompression
                        |> UnionFind.union 0 1
                        |> UnionFind.union 1 2
                        |> UnionFind.union 2 3
                        |> UnionFind.union 0 4
                        |> Expect.all
                            [ UnionFind.get 0 >> Expect.equal 3
                            , UnionFind.get 1 >> Expect.equal 3
                            , UnionFind.get 2 >> Expect.equal 3
                            , UnionFind.get 3 >> Expect.equal 4
                            , UnionFind.get 4 >> Expect.equal 4
                            ]
            ]
        , test "example from https://algs4.cs.princeton.edu/15uf/" <|
            \() ->
                let
                    getState : UnionFind Int -> List Int
                    getState uf =
                        List.map
                            (\id -> UnionFind.get id uf)
                            (List.range 0 9)

                    step : ( Int, Int, List Int ) -> ( UnionFind Int, List (List Int), List (List Int) ) -> ( UnionFind Int, List (List Int), List (List Int) )
                    step ( left, right, expectation ) ( uf, accResult, accExpect ) =
                        let
                            nextUF =
                                UnionFind.union left right uf
                        in
                        ( nextUF, getState nextUF :: accResult, expectation :: accExpect )

                    ( _, result, expect ) =
                        [ ( 4, 3, [ 0, 1, 2, 3, 3, 5, 6, 7, 8, 9 ] )
                        , ( 3, 8, [ 0, 1, 2, 8, 3, 5, 6, 7, 8, 9 ] )
                        , ( 6, 5, [ 0, 1, 2, 8, 3, 5, 5, 7, 8, 9 ] )
                        , ( 9, 4, [ 0, 1, 2, 8, 8, 5, 5, 7, 8, 8 ] )
                        , ( 2, 1, [ 0, 1, 1, 8, 8, 5, 5, 7, 8, 8 ] )
                        , ( 8, 9, [ 0, 1, 1, 8, 8, 5, 5, 7, 8, 8 ] )
                        , ( 5, 0, [ 0, 1, 1, 8, 8, 0, 5, 7, 8, 8 ] )
                        , ( 7, 2, [ 0, 1, 1, 8, 8, 0, 5, 1, 8, 8 ] )
                        , ( 6, 1, [ 1, 1, 1, 8, 8, 0, 0, 1, 8, 8 ] )
                        , ( 1, 0, [ 1, 1, 1, 8, 8, 0, 0, 1, 8, 8 ] )
                        , ( 6, 7, [ 1, 1, 1, 8, 8, 0, 1, 1, 8, 8 ] )
                        , ( 5, 9, [ 1, 8, 1, 8, 8, 1, 1, 1, 8, 8 ] )
                        ]
                            |> List.reverse
                            |> List.foldr step ( UnionFind.quickUnionPathCompression, [], [] )
                in
                Expect.equal result expect
        ]
