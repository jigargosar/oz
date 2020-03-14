module Forest exposing (Forest)

import Forest.Tree exposing (Tree)


type alias Forest a =
    List (Tree a)
