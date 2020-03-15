module Forest exposing (restructure)

import Forest.Tree as Tree exposing (Forest)


restructure : (a -> b) -> (b -> List c -> c) -> Forest a -> List c
restructure fData fTree =
    List.map (Tree.restructure fData fTree)
