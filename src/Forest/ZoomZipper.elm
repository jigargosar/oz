module Forest.Zipper exposing (ZoomZipper, appendChildGo, down, fromData, insertLeftGo, insertRightGo, left, prependChildGo, right, up)

import Forest.Tree as T exposing (Forest, Tree)


type TreeListZipper a
    = TLZ (List (Tree a)) (Tree a) (List (Tree a))


fromCR : Tree a -> List (Tree a) -> TreeListZipper a
fromCR c r =
    TLZ [] c r


fromCL : Tree a -> List (Tree a) -> TreeListZipper a
fromCL c l =
    TLZ (List.reverse l) c []


toList : TreeListZipper a -> List (Tree a)
toList (TLZ lfr c rf) =
    List.reverse lfr ++ c :: rf



-- FOREST ZIPPER MODEL


type ZoomZipper a
    = ZoomZipper (List (Crumb a)) (List (Crumb a)) (TreeListZipper a)


type Crumb a
    = Crumb (Forest a) a (Forest a)


fromData : a -> ZoomZipper a
fromData a =
    ZoomZipper [] [] (TLZ [] (T.singleton a) [])



-- Nav


right : ZoomZipper a -> Maybe (ZoomZipper a)
right (ZoomZipper pcs cs (TLZ lfr c rf)) =
    case rf of
        [] ->
            Nothing

        first :: rest ->
            Just (ZoomZipper pcs cs (TLZ (c :: lfr) first rest))


left : ZoomZipper a -> Maybe (ZoomZipper a)
left (ZoomZipper pcs cs (TLZ lfr c rf)) =
    case lfr of
        [] ->
            Nothing

        first :: rest ->
            Just (ZoomZipper pcs cs (TLZ rest first (c :: rf)))


up : ZoomZipper a -> Maybe (ZoomZipper a)
up (ZoomZipper pcs crumbs tlz) =
    case crumbs of
        [] ->
            Nothing

        (Crumb l data r) :: rest ->
            TLZ l (T.tree data (toList tlz)) r
                |> ZoomZipper pcs rest
                |> Just


down : ZoomZipper a -> Maybe (ZoomZipper a)
down (ZoomZipper pcs cs (TLZ l c r)) =
    case ( T.data c, T.children c ) of
        ( data, firstChild :: rest ) ->
            ZoomZipper pcs (Crumb l data r :: cs) (fromCR firstChild rest)
                |> Just

        _ ->
            Nothing



-- ADD NEW


insertRightGo : Tree a -> ZoomZipper a -> ZoomZipper a
insertRightGo node (ZoomZipper pcs cs (TLZ l c r)) =
    ZoomZipper pcs cs (TLZ (c :: l) node r)


insertLeftGo : Tree a -> ZoomZipper a -> ZoomZipper a
insertLeftGo node (ZoomZipper pcs cs (TLZ l c r)) =
    ZoomZipper pcs cs (TLZ l node (c :: r))


appendChildGo : Tree a -> ZoomZipper a -> ZoomZipper a
appendChildGo node (ZoomZipper pcs cs (TLZ lfr c rf)) =
    let
        crumb =
            Crumb lfr (T.data c) rf

        tlz =
            fromCL node (T.children c)
    in
    ZoomZipper pcs (crumb :: cs) tlz


prependChildGo : Tree a -> ZoomZipper a -> ZoomZipper a
prependChildGo node (ZoomZipper pcs cs (TLZ lfr c rf)) =
    let
        crumb =
            Crumb lfr (T.data c) rf

        tlz =
            fromCR node (T.children c)
    in
    ZoomZipper pcs (crumb :: cs) tlz
