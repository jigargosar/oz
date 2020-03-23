module Forest.ZoomZipper exposing
    ( ZoomZipper
    , appendChildGo
    , down
    , findFirst
    , fromCons
    , fromData
    , fromForest
    , insertLeftGo
    , insertRightGo
    , left
    , prependChildGo
    , resetZoom
    , right
    , rootForest
    , unConsRoot
    , up
    , zoomIn
    )

import Forest.Tree as T exposing (Forest, Tree)
import Utils exposing (..)


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


fromForest : Forest a -> Maybe (ZoomZipper a)
fromForest ls =
    case ls of
        [] ->
            Nothing

        f :: r ->
            fromCons f r |> Just


fromCons : Tree a -> Forest a -> ZoomZipper a
fromCons f r =
    fromCR f r |> ZoomZipper [] []


unConsRoot : ZoomZipper a -> ( Tree a, Forest a )
unConsRoot =
    resetZoom
        >> applyWhileJust up
        >> unCons


unCons : ZoomZipper a -> ( Tree a, Forest a )
unCons (ZoomZipper _ _ ((TLZ _ c _) as tlz)) =
    case toList tlz of
        [] ->
            ( c, [] )

        f :: r ->
            ( f, r )


rootForest : ZoomZipper a -> Forest a
rootForest =
    unConsRoot >> uncurry (::)



-- ZOOM


zoomIn : ZoomZipper a -> Maybe (ZoomZipper a)
zoomIn =
    down >> Maybe.map (\(ZoomZipper pcs cs tlz) -> ZoomZipper (cs ++ pcs) [] tlz)


resetZoom : ZoomZipper a -> ZoomZipper a
resetZoom (ZoomZipper pcs cs tlz) =
    ZoomZipper [] (cs ++ pcs) tlz



-- ACCESS


data (ZoomZipper _ _ (TLZ _ c _)) =
    T.data c



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
            TLZ rest first (c :: rf)
                |> ZoomZipper pcs cs
                |> Just


up : ZoomZipper a -> Maybe (ZoomZipper a)
up (ZoomZipper pcs crumbs tlz) =
    case crumbs of
        [] ->
            Nothing

        (Crumb l data_ r) :: rest ->
            TLZ l (T.tree data_ (toList tlz)) r
                |> ZoomZipper pcs rest
                |> Just


down : ZoomZipper a -> Maybe (ZoomZipper a)
down (ZoomZipper pcs cs (TLZ l c r)) =
    case ( T.data c, T.children c ) of
        ( data_, firstChild :: rest ) ->
            ZoomZipper pcs (Crumb l data_ r :: cs) (fromCR firstChild rest)
                |> Just

        _ ->
            Nothing


forward : ZoomZipper a -> Maybe (ZoomZipper a)
forward =
    firstOf [ down, right, rightOfAncestor ]


rightOfAncestor : ZoomZipper a -> Maybe (ZoomZipper a)
rightOfAncestor z =
    case up z of
        Nothing ->
            Nothing

        Just pz ->
            case right pz of
                Just rpz ->
                    Just rpz

                Nothing ->
                    rightOfAncestor pz



-- FIND


findFirst : (a -> Bool) -> ZoomZipper a -> Maybe (ZoomZipper a)
findFirst pred =
    applyWhileJust up >> applyWhileJust left >> find pred forward


find :
    (a -> Bool)
    -> (ZoomZipper a -> Maybe (ZoomZipper a))
    -> ZoomZipper a
    -> Maybe (ZoomZipper a)
find pred nextFunc z =
    if pred (data z) then
        Just z

    else
        case nextFunc z of
            Just nz ->
                find pred nextFunc nz

            Nothing ->
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
