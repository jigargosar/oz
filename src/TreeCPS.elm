port module TreeCPS exposing (..)

-- CPS VISIT

fzVisit2 :
    { enter : ForestZipper a -> acc -> acc
    , exit : ForestZipper a -> acc -> acc
    }
    -> acc
    -> ForestZipper a
    -> acc
fzVisit2 { enter, exit } =
    let
        exitParentsTillRight : acc -> ForestZipper a -> ( acc, Maybe (ForestZipper a) )
        exitParentsTillRight acc oz =
            let
                exitAcc =
                    exit oz acc
            in
            case right oz of
                Just rightOZ ->
                    ( exitAcc, Just rightOZ )

                Nothing ->
                    case up oz of
                        Just parentOZ ->
                            exitParentsTillRight exitAcc parentOZ

                        Nothing ->
                            ( exitAcc, Nothing )

        visitHelp : acc -> ForestZipper a -> acc
        visitHelp acc0 oz =
            let
                enterAcc =
                    enter oz acc0
            in
            case down oz of
                Just childOZ ->
                    visitHelp enterAcc childOZ

                Nothing ->
                    let
                        exitAcc =
                            exit oz enterAcc
                    in
                    case right oz of
                        Just rightSiblingOZ ->
                            visitHelp exitAcc rightSiblingOZ

                        Nothing ->
                            case up oz of
                                Nothing ->
                                    exitAcc

                                Just poz ->
                                    case exitParentsTillRight exitAcc poz of
                                        ( parentExitAcc, Just rightOZ ) ->
                                            visitHelp parentExitAcc rightOZ

                                        ( parentExitAcc, Nothing ) ->
                                            parentExitAcc

        startVisitHelp : acc -> ForestZipper a -> acc
        startVisitHelp acc fz =
            visitHelp acc (firstRoot fz)
    in
    startVisitHelp


-- VISIT
fzVisit :
    { enter : ForestZipper a -> acc -> acc
    , exit : ForestZipper a -> acc -> acc
    }
    -> acc
    -> ForestZipper a
    -> acc
fzVisit { enter, exit } =
    let
        exitParentsTillRight : acc -> ForestZipper a -> ( acc, Maybe (ForestZipper a) )
        exitParentsTillRight acc oz =
            let
                exitAcc =
                    exit oz acc
            in
            case right oz of
                Just rightOZ ->
                    ( exitAcc, Just rightOZ )

                Nothing ->
                    case up oz of
                        Just parentOZ ->
                            exitParentsTillRight exitAcc parentOZ

                        Nothing ->
                            ( exitAcc, Nothing )

        visitHelp : acc -> ForestZipper a -> acc
        visitHelp acc0 oz =
            let
                enterAcc =
                    enter oz acc0
            in
            case down oz of
                Just childOZ ->
                    visitHelp enterAcc childOZ

                Nothing ->
                    let
                        exitAcc =
                            exit oz enterAcc
                    in
                    case right oz of
                        Just rightSiblingOZ ->
                            visitHelp exitAcc rightSiblingOZ

                        Nothing ->
                            case up oz of
                                Nothing ->
                                    exitAcc

                                Just poz ->
                                    case exitParentsTillRight exitAcc poz of
                                        ( parentExitAcc, Just rightOZ ) ->
                                            visitHelp parentExitAcc rightOZ

                                        ( parentExitAcc, Nothing ) ->
                                            parentExitAcc

        startVisitHelp : acc -> ForestZipper a -> acc
        startVisitHelp acc fz =
            visitHelp acc (firstRoot fz)
    in
    startVisitHelp


-- TREE

import Maybe.Extra


type Tree a
    = Tree a (List (Tree a))


mapTreeData : (a -> a) -> Tree a -> Tree a
mapTreeData func (Tree a children) =
    Tree (func a) children


treeData : Tree a -> a
treeData (Tree a _) =
    a


type alias Forest a =
    List (Tree a)



-- FOREST ZIPPER


type alias ForestZipper a =
    { leftReversed : Forest a
    , center : Tree a
    , right_ : Forest a
    , crumbs : List (Crumb a)
    }


type alias Crumb a =
    { leftReversed : Forest a
    , datum : a
    , right_ : Forest a
    }


fromForest : Forest a -> Maybe (ForestZipper a)
fromForest forest =
    case forest of
        [] ->
            Nothing

        first :: rest ->
            Just { leftReversed = [], center = first, right_ = rest, crumbs = [] }


withRollback : (ForestZipper a -> Maybe (ForestZipper a)) -> ForestZipper a -> ForestZipper a
withRollback func oz =
    func oz |> Maybe.withDefault oz


mapTree : (Tree a -> Tree a) -> ForestZipper a -> ForestZipper a
mapTree func fz =
    { fz | center = func fz.center }


fzMapData : (a -> a) -> ForestZipper a -> ForestZipper a
fzMapData func =
    mapTree (mapTreeData func)


getTree : ForestZipper a -> Tree a
getTree fz =
    fz.center


getLevel : ForestZipper a -> Int
getLevel fz =
    List.length fz.crumbs



--isFirst : ForestZipper a -> Bool
--isFirst fz =
--    List.isEmpty fz.leftReversed
--
--
--isLast : ForestZipper a -> Bool
--isLast fz =
--    List.isEmpty fz.right_
--
-- CORE NAVIGATION


{-|

    @docs up, down, left, right.
    alternative names:
        ascend, descend, before, after, respectively.

    Alternatives names for: left/right:
        before/after; previousSibling/nextSibling.

-}
up : ForestZipper a -> Maybe (ForestZipper a)
up acc =
    case acc.crumbs of
        [] ->
            Nothing

        { leftReversed, datum, right_ } :: rest ->
            Just
                { acc
                    | leftReversed = leftReversed
                    , center = Tree datum (List.reverse acc.leftReversed ++ acc.center :: acc.right_)
                    , right_ = right_
                    , crumbs = rest
                }


down : ForestZipper a -> Maybe (ForestZipper a)
down acc =
    case acc.center of
        Tree _ [] ->
            Nothing

        Tree a (first :: rest) ->
            Just
                { acc
                    | leftReversed = []
                    , center = first
                    , right_ = rest
                    , crumbs =
                        { leftReversed = acc.leftReversed, datum = a, right_ = acc.right_ }
                            :: acc.crumbs
                }


left : ForestZipper a -> Maybe (ForestZipper a)
left acc =
    case acc.leftReversed of
        [] ->
            Nothing

        first :: rest ->
            Just
                { acc
                    | leftReversed = rest
                    , center = first
                    , right_ = acc.center :: acc.right_
                }


right : ForestZipper a -> Maybe (ForestZipper a)
right acc =
    case acc.right_ of
        [] ->
            Nothing

        first :: rest ->
            Just
                { acc
                    | leftReversed = acc.center :: acc.leftReversed
                    , center = first
                    , right_ = rest
                }



-- NAVIGATE


applyWhileJust : (a -> Maybe a) -> a -> a
applyWhileJust func a =
    case func a of
        Just a2 ->
            applyWhileJust func a2

        Nothing ->
            a


root : ForestZipper a -> ForestZipper a
root =
    applyWhileJust up


firstRoot : ForestZipper a -> ForestZipper a
firstRoot =
    root >> applyWhileJust left


nextSiblingOfClosestAncestor : ForestZipper a -> Maybe (ForestZipper a)
nextSiblingOfClosestAncestor acc =
    case up acc of
        Just parentAcc ->
            case right parentAcc of
                Just ns ->
                    Just ns

                Nothing ->
                    nextSiblingOfClosestAncestor parentAcc

        Nothing ->
            Nothing


findFromCurrent : (a -> Bool) -> ForestZipper a -> Maybe (ForestZipper a)
findFromCurrent pred acc =
    case acc.center of
        Tree a _ ->
            if pred a then
                Just acc

            else
                case Maybe.Extra.oneOf [ down, right, nextSiblingOfClosestAncestor ] acc of
                    Just nextAcc ->
                        findFromCurrent pred nextAcc

                    Nothing ->
                        Nothing


findFirst : (a -> Bool) -> ForestZipper a -> Maybe (ForestZipper a)
findFirst pred acc =
    firstRoot acc |> findFromCurrent pred



-- INSERTION


insertLeft : Tree a -> ForestZipper a -> ForestZipper a
insertLeft tree acc =
    { acc | leftReversed = tree :: acc.leftReversed }


insertRight : Tree a -> ForestZipper a -> ForestZipper a
insertRight tree acc =
    { acc | right_ = tree :: acc.right_ }


insertFirstChild : Tree a -> ForestZipper a -> ForestZipper a
insertFirstChild child acc =
    case acc.center of
        Tree a children ->
            { acc | center = Tree a (child :: children) }


insertLastChild : Tree a -> ForestZipper a -> ForestZipper a
insertLastChild child acc =
    case acc.center of
        Tree a children ->
            { acc | center = Tree a (children ++ [ child ]) }



-- DELETION


remove : ForestZipper a -> Maybe (ForestZipper a)
remove acc =
    case ( acc.leftReversed, acc.right_ ) of
        ( [], [] ) ->
            case acc.crumbs of
                [] ->
                    --Nothing
                    Debug.todo "should not happen"

                { leftReversed, datum, right_ } :: rest ->
                    Just
                        { acc
                            | leftReversed = leftReversed
                            , center = Tree datum []
                            , right_ = right_
                            , crumbs = rest
                        }

        ( first :: rest, _ ) ->
            Just { acc | leftReversed = rest, center = first }

        ( _, first :: rest ) ->
            Just { acc | center = first, right_ = rest }



-- UTILS


propEq : (c -> b) -> b -> c -> Bool
propEq func val obj =
    func obj == val
