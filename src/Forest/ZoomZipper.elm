module Forest.Zipper exposing (ZoomZipper, appendChildGo, fromData, insertLeftGo, insertRightGo, left, prependChildGo, right)

import Forest.Tree as T exposing (Forest, Tree)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Utils exposing (..)



-- FOREST ZIPPER MODEL


type ZoomZipper a
    = ZoomZipper (List (Crumb a)) (List (Crumb a)) (Forest a) (Tree a) (Forest a)


type Crumb a
    = Crumb (Forest a) a (Forest a)


fromData : a -> ZoomZipper a
fromData a =
    ZoomZipper [] [] [] (T.singleton a) []



-- Nav


right : ZoomZipper a -> Maybe (ZoomZipper a)
right (ZoomZipper pcs cs lfr c rf) =
    case rf of
        [] ->
            Nothing

        first :: rest ->
            Just (ZoomZipper pcs cs (c :: lfr) first rest)


left : ZoomZipper a -> Maybe (ZoomZipper a)
left (ZoomZipper pcs cs lfr c rf) =
    case lfr of
        [] ->
            Nothing

        first :: rest ->
            Just (ZoomZipper pcs cs rest first (c :: rf))



-- ADD NEW


insertRightGo : Tree a -> ZoomZipper a -> ZoomZipper a
insertRightGo node (ZoomZipper pcs cs lfr c rf) =
    ZoomZipper pcs cs (c :: lfr) node rf


insertLeftGo : Tree a -> ZoomZipper a -> ZoomZipper a
insertLeftGo node (ZoomZipper pcs cs lfr c rf) =
    ZoomZipper pcs cs lfr node (c :: rf)


appendChildGo : Tree a -> ZoomZipper a -> ZoomZipper a
appendChildGo node (ZoomZipper pcs cs lfr c rf) =
    ZoomZipper pcs
        (Crumb lfr (T.data c) rf :: cs)
        (T.children c |> List.reverse)
        node
        []


prependChildGo : Tree a -> ZoomZipper a -> ZoomZipper a
prependChildGo node (ZoomZipper pcs cs lfr c rf) =
    ZoomZipper pcs
        (Crumb lfr (T.data c) rf :: cs)
        []
        node
        (T.children c)
