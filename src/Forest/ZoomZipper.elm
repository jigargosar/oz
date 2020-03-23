module Forest.Zipper exposing (ZoomZipper, appendChildGo, down, fromData, insertLeftGo, insertRightGo, left, prependChildGo, right, up)

import Forest.Tree as T exposing (Forest, Tree)


type TreeListZipper a
    = TLZ (List (Tree a)) (Tree a) (List (Tree a))


tlzFromCR : Tree a -> List (Tree a) -> TreeListZipper a
tlzFromCR c r =
    TLZ [] c r


tlzToList : TreeListZipper a -> List (Tree a)
tlzToList (TLZ lfr c rf) =
    List.reverse lfr ++ c :: rf


tlzToTree : a -> TreeListZipper a -> Tree a
tlzToTree data tlz =
    T.tree data (tlzToList tlz)



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
up (ZoomZipper pcs cs tlz) =
    case cs of
        [] ->
            Nothing

        first :: rest ->
            Just (ZoomZipper pcs rest (construct first tlz))


down : ZoomZipper a -> Maybe (ZoomZipper a)
down (ZoomZipper pcs cs tlz) =
    case deconstruct tlz of
        Just ( crumb, newTLZ ) ->
            Just (ZoomZipper pcs (crumb :: cs) newTLZ)

        Nothing ->
            Nothing


construct : Crumb a -> TreeListZipper a -> TreeListZipper a
construct (Crumb l d r) tlz =
    TLZ l (tlzToTree d tlz) r


deconstruct : TreeListZipper a -> Maybe ( Crumb a, TreeListZipper a )
deconstruct (TLZ lfr c rf) =
    case T.children c of
        [] ->
            Nothing

        first :: rest ->
            Just ( Crumb lfr (T.data c) rf, tlzFromCR first rest )



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
            tlzFromCL node (T.children c)
    in
    ZoomZipper pcs (crumb :: cs) tlz


tlzFromCL c l =
    TLZ (List.reverse l) c []


prependChildGo : Tree a -> ZoomZipper a -> ZoomZipper a
prependChildGo node (ZoomZipper pcs cs (TLZ lfr c rf)) =
    let
        crumb =
            Crumb lfr (T.data c) rf

        tlz =
            tlzFromCR node (T.children c)
    in
    ZoomZipper pcs (crumb :: cs) tlz
