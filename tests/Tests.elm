module Tests exposing (quickUnionPathCompression)

import Expect exposing (Expectation)
import Test exposing (Test, describe, test)
import UnionFind


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
        ]
