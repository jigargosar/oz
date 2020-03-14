module Forest exposing (Forest, restructure)

import Forest.Tree as Tree exposing (Forest)


type alias Forest a =
    Tree.Forest a


restructure : (a -> b) -> (b -> List c -> c) -> Forest a -> List c
restructure fData fTree =
    List.map (Tree.restructure fData fTree)
