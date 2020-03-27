module Forest.Tree exposing
    ( Forest
    , Tree
    , children
    , data
    , foldl
    , map
    , mapChildren
    , mapData
    , restructure
    , singleton
    , tree
    )

import Tree exposing (Tree)


type alias Tree a =
    Tree.Tree a


type alias Forest a =
    List (Tree a)


tree : a -> List (Tree a) -> Tree a
tree =
    Tree.tree


singleton : a -> Tree a
singleton =
    Tree.singleton


mapData : (a -> a) -> Tree a -> Tree a
mapData =
    Tree.mapLabel


mapChildren : (Forest a -> Forest a) -> Tree a -> Tree a
mapChildren =
    Tree.mapChildren


data : Tree a -> a
data =
    Tree.label


children : Tree a -> Forest a
children =
    Tree.children


restructure : (a -> b) -> (b -> List c -> c) -> Tree a -> c
restructure =
    Tree.restructure


map : (a -> b) -> Tree a -> Tree b
map =
    Tree.map


foldl : (a -> b -> b) -> b -> Tree a -> b
foldl =
    Tree.foldl
