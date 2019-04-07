# Union-Find in Elm

This package will help you to solve dynamic connectivity problem
using the Union-Find data structure.
If you would like to know more about the topic check the 
[Algorithms](https://algs4.cs.princeton.edu/15uf) sitebook by Robert Sedgewick and Kevin Wayne.

There are some differences between API described in the book and current implementation, such as:
- `UnionFind.count` returns not amount of components but amount of connections.
  The reason is in the implementation there is no need to initialise Union-Find instance 
  with an array of values. You could call `UnionFind.union` with any valid arguments and then 
  count amount components as `size - count`, where size is amount of elements.
- `UnionFind.get` returns the parent of element.
  It's just a way to get incapsulated values from the UF instance.


## Connectivity Example

Given a matrix of values from 0 to 9.

```
(0)   (1)   (2)   (3)   (4)

(5)   (6)   (7)   (8)   (9)
```

Let's make some connections between them:

```
|=====|   (0)   (1)   (2)   (3) - (4)
| 4 3 |
|=====|   (5)   (6)   (7)   (8)   (9)

|=====|   (0)   (1)   (2)   (3) - (4)
| 3 8 |                      |
|=====|   (5)   (6)   (7)   (8)   (9)

|=====|   (0)   (1)   (2)   (3) - (4)
| 6 5 |                      |
|=====|   (5) - (6)   (7)   (8)   (9)

|=====|   (0)   (1)   (2)   (3) - (4)
| 9 4 |                      |     |
|=====|   (5) - (6)   (7)   (8)   (9)

|=====|   (0)   (1) _ (2)   (3) - (4)
| 2 1 |                      |     |
|=====|   (5) - (6)   (7)   (8)   (9)

|-----|   (0)   (1) - (2)   (3) - (4)
| 8 9 |                      |     |
|-----|   (5) - (6)   (7)   (8)   (9)

|=====|   (0)   (1) - (2)   (3) - (4)
| 5 0 |    |                 |     |
|=====|   (5) - (6)   (7)   (8)   (9)

|=====|   (0)   (1) - (2)   (3) - (4)
| 7 2 |    |           |     |     |
|=====|   (5) - (6)   (7)   (8)   (9)

|=====|   (0)   (1) - (2)   (3) - (4)
| 6 1 |    |     |     |     |     |
|=====|   (5) - (6)   (7)   (8)   (9)

|-----|   (0)   (1) - (2)   (3) - (4)
| 1 0 |    |     |     |     |     |
|-----|   (5) - (6)   (7)   (8)   (9)

|-----|   (0)   (1) - (2)   (3) - (4)
| 6 7 |    |     |     |     |     |
|-----|   (5) - (6)   (7)   (8)   (9)

|=====|   (0)   (1) - (2)   (3) - (4)
| 5 9 |    |     |     |     |     |
|=====| - (5) - (6)   (7)   (8)   (9) -
```

As you can see there is no need to make connections (8, 9), (1, 0) and (6, 7)
because the elements were already connected thru their neighbours.
At the and we've made connections between all of the elements, 
so amount of connections (`UnionFind.coun`) is `9` 
and amount of components (`size - count` where size is amount of elements) is `1`.

## Quick Union Path Compression

Current implementation is based on Quick Union Path Compression algorithm.
Let's reproduce same example as behind but using the impl.

```elm
uf : Int
uf =
    UnionFind.quickUnionPathCompression
        |> UnionFind.union 4 3
        |> UnionFind.union 3 8
        |> UnionFind.union 6 5
        |> UnionFind.union 9 4
        |> UnionFind.union 2 1
        |> UnionFind.union 8 9
        |> UnionFind.union 5 0
        |> UnionFind.union 7 2
        |> UnionFind.union 6 1
        |> UnionFind.union 1 0
        |> UnionFind.union 6 7
        |> UnionFind.union 5 9
        |> UnionFind.count      -- equal 1
```

The algorithm works like this:

```
._________________________________________________________________.
|     |                                                           |
|     | (0)   (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   (9) |
|_____|___________________________________________________________|
|     |                                                           |
| 4 3 | (0)   (1)   (2)   (3)   (5)   (6)   (7)   (8)   (9)       |
|     |                    |                                      |
|     |                   (4)                                     |
|_____|___________________________________________________________|
|     |                                                           |
| 3 8 | (0)   (1)   (2)   (5)   (6)   (7)   (8)   (9)             |
|     |                                      |                    |
|     |                                     (3)                   |
|     |                                      |                    |
|     |                                     (4)                   |
|_____|___________________________________________________________|
|     |                                                           |
| 6 5 | (0)   (1)   (2)   (5)   (7)   (8)   (9)                   |
|     |                    |           |                          |
|     |                   (6)         (3)                         |
|     |                                |                          |
|     |                               (4)                         |
|_____|___________________________________________________________|
|     |                                                           |
| 9 4 | (0)   (1)   (2)   (5)   (7)  .(8).                        |
|     |                    |        /  |  \                       |
|     |                   (6)     (3) (4) (9)                     |
|_____|___________________________________________________________|
|     |                                                           |
| 2 1 | (0)   (1)   (5)   (7)  .(8).                              |
|     |        |     |        /  |  \                             |
|     |       (2)   (6)     (3) (4) (9)                           |
|_____|___________________________________________________________|
|     |                                                           |
| 8 9 | (0)   (1)   (5)   (7)  .(8).                              |
|     |        |     |        /  |  \                             |
|     |       (2)   (6)     (3) (4) (9)                           |
|_____|___________________________________________________________|
|     |                                                           |
| 5 0 | (0)   (1)   (7)  .(8).                                    |
|     |  |     |        /  |  \                                   |
|     | (5)   (2)     (3) (4) (9)                                 |
|     |  |                                                        |
|     | (6)                                                       |
|_____|___________________________________________________________|
|     |                                                           |
| 7 2 | (0)   (1).         .(8).                                  |
|     |  |     |  \       /  |  \                                 |
|     | (5)   (2) (7)   (3) (4) (9)                               |
|     |  |                                                        |
|     | (6)                                                       |
|_____|___________________________________________________________|
|     |                                                           |
| 6 1 |        .(1).         .(8).                                |
|     |       /  |  \       /  |  \                               |
|     |    .(0) (2) (7)   (3) (4) (9)                             |
|     |   /  |                                                    |
|     | (5) (6)                                                   |
|_____|___________________________________________________________|
|     |                                                           |
| 1 0 |        .(1).         .(8).                                |
|     |       /  |  \       /  |  \                               |
|     |    .(0) (2) (7)   (3) (4) (9)                             |
|     |   /  |                                                    |
|     | (5) (6)                                                   |
|_____|___________________________________________________________|
|     |                                                           |
| 6 7 |      .(1).___.         .(8).                              |
|     |     /  |  \   \       /  |  \                             |
|     |   (0) (2) (6) (7)   (3) (4) (9)                           |
|     |    |                                                      |
|     |   (5)                                                     |
|_____|___________________________________________________________|
|     |                                                           |
| 5 9 |                 (8)___.___.                               |
|     |                /   \   \   \                              |
|     |     .___.___.(1).  (3) (4) (9)                            |
|     |    /   /   /  |  \                                        |
|     |  (0) (2) (5) (6) (7)                                      |
|_____|___________________________________________________________|
```

As you can notice the idea is connect grandchild to parent each time
when the grandchild is going to be connected with another element
even if the elements are already connected.
