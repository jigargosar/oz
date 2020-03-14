module Forest.Tree exposing
    ( Forest
    , Tree
    , children
    , data
    , leaf
    , mapData
    , toTuple
    , tree
    )

import Tree exposing (Tree)


type alias Tree a =
    Tree.Tree a


type alias Forest a =
    List (Tree a)


leaf : a -> Tree a
leaf a =
    tree a []


tree : a -> List (Tree a) -> Tree a
tree =
    Tree.tree


mapData : (a -> a) -> Tree a -> Tree a
mapData =
    Tree.mapLabel


data : Tree a -> a
data =
    Tree.label


children : Tree a -> Forest a
children =
    Tree.children


toTuple : Tree a -> ( a, Forest a )
toTuple t =
    ( data t, children t )
