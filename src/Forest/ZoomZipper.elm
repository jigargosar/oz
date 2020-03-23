module Forest.Zipper exposing (ZoomZipper, appendChildGo, down, fromData, insertLeftGo, insertRightGo, left, prependChildGo, right)

import Forest.Tree as T exposing (Forest, Tree)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Utils exposing (..)


type TreeListZipper a
    = TLZ (List (Tree a)) (Tree a) (List (Tree a))


tlzFromTree : Tree a -> TreeListZipper a
tlzFromTree t =
    TLZ [] t []


tlzSingleton : a -> TreeListZipper a
tlzSingleton a =
    T.singleton a |> tlzFromTree

tlzFromCR: Tree a -> List (Tree a) -> TreeListZipper a
tlzFromCR c r =
    TLZ [] c r

tlzFromList : List (Tree a) -> Maybe (TreeListZipper a)
tlzFromList ls =
    case ls of
        [] ->
            Nothing

        f :: r ->
            Just (tlzFromCR f r)


tlzFromChildrenOf : Tree a -> Maybe (TreeListZipper a)
tlzFromChildrenOf t =
    tlzFromList (T.children t)


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
down (ZoomZipper pcs cs (TLZ lfr c rf)) =
    tlzFromChildrenOf c
        |> Maybe.map (ZoomZipper pcs (Crumb lfr (T.data c) rf :: cs))


construct : Crumb a -> TreeListZipper a -> TreeListZipper a
construct (Crumb l d r) tlz =
    TLZ l (tlzToTree d tlz) r

deconstruct: TreeListZipper a -> Maybe (Crumb a, TreeListZipper a)
deconstruct (TLZ lfr c rf) =
    tlzFromList (T.children c)
        |> Maybe.


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
