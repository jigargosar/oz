module Forest.Zipper exposing (Zipper, fromForest)

import Forest exposing (Forest)
import Tree.Zipper exposing (Zipper)


type alias Zipper a =
    Tree.Zipper.Zipper a


fromForest : Forest a -> Maybe (Zipper a)
fromForest forest =
    case forest of
        first :: rest ->
            Just (Tree.Zipper.fromForest first rest)

        [] ->
            Nothing
