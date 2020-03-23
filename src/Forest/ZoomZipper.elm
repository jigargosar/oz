module Forest.Zipper exposing (ZoomZipper, appendChildGo, down, fromData, insertLeftGo, insertRightGo, left, prependChildGo, right, up)

import Forest.Tree as T exposing (Forest, Tree)


type TreeListZipper a
    = TLZ (List (Tree a)) (Tree a) (List (Tree a))


tlzFromTree : Tree a -> TreeListZipper a
tlzFromTree t =
    TLZ [] t []


tlzSingleton : a -> TreeListZipper a
tlzSingleton a =
    T.singleton a |> tlzFromTree


tlzFromCR : Tree a -> List (Tree a) -> TreeListZipper a
tlzFromCR c r =
    TLZ [] c r


tlzToList : TreeListZipper a -> List (Tree a)
tlzToList (TLZ lfr c rf) =
    List.reverse lfr ++ c :: rf


tlzToTree : a -> TreeListZipper a -> Tree a
tlzToTree data tlz =
    T.tree data (tlzToList tlz)


tlzAddRightGo : Tree a -> TreeListZipper a -> TreeListZipper a
tlzAddRightGo t (TLZ l c r) =
    TLZ (c :: l) t r


tlzAddLeftGo : Tree a -> TreeListZipper a -> TreeListZipper a
tlzAddLeftGo t (TLZ l c r) =
    TLZ l t (c :: r)



-- FOREST ZIPPER MODEL


type ZoomZipper a
    = ZoomZipper (List (Crumb a)) (List (Crumb a)) (TreeListZipper a)


type Crumb a
    = Crumb (Forest a) a (Forest a)


crumbFromTLZ : TreeListZipper a -> Crumb a
crumbFromTLZ (TLZ l c r) =
    Crumb l (T.data c) r


fromData : a -> ZoomZipper a
fromData a =
    ZoomZipper [] [] (tlzSingleton a)



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
insertRightGo node (ZoomZipper pcs cs tlz) =
    ZoomZipper pcs cs (tlzAddRightGo node tlz)


insertLeftGo : Tree a -> ZoomZipper a -> ZoomZipper a
insertLeftGo node (ZoomZipper pcs cs tlz) =
    ZoomZipper pcs cs (tlzAddLeftGo node tlz)


appendChildGo : Tree a -> ZoomZipper a -> ZoomZipper a
appendChildGo node (ZoomZipper pcs cs ((TLZ lfr c rf) as tlz)) =
    ZoomZipper pcs
        (crumbFromTLZ tlz :: cs)
        (TLZ (T.children c |> List.reverse)
            node
            []
        )


prependChildGo : Tree a -> ZoomZipper a -> ZoomZipper a
prependChildGo node (ZoomZipper pcs cs ((TLZ lfr c rf) as tlz)) =
    ZoomZipper pcs
        (crumbFromTLZ tlz :: cs)
        (TLZ []
            node
            (T.children c)
        )
