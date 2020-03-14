module Tree exposing (Tree)

import Maybe.Extra



-- FOREST TRANSFORM


type alias ForestTransformZipper a ctx tree =
    { leftReversed : List tree
    , context : ctx
    , crumbs : List { leftReversed : List tree, center : ( a, ctx ), right : Forest a }
    }


type alias TransformForestConfig a ctx tree =
    { transform : a -> ctx -> List tree -> tree
    , childContext : a -> ctx -> ctx
    }


transformForestWithContext : TransformForestConfig a ctx tree -> ctx -> Forest a -> List tree
transformForestWithContext cfg =
    let
        build : Forest a -> ForestTransformZipper a ctx tree -> List tree
        build rightForest z =
            case rightForest of
                first :: rest ->
                    let
                        data =
                            treeData first

                        childCtx : ctx
                        childCtx =
                            cfg.childContext data z.context
                    in
                    build
                        (treeChildren first)
                        { leftReversed = []
                        , context = childCtx
                        , crumbs =
                            { leftReversed = z.leftReversed
                            , center = ( data, z.context )
                            , right = rest
                            }
                                :: z.crumbs
                        }

                [] ->
                    case z.crumbs of
                        parentCrumb :: rest ->
                            build
                                parentCrumb.right
                                { leftReversed =
                                    cfg.transform (Tuple.first parentCrumb.center)
                                        (Tuple.second parentCrumb.center)
                                        (List.reverse z.leftReversed)
                                        :: parentCrumb.leftReversed
                                , context = Tuple.second parentCrumb.center
                                , crumbs = rest
                                }

                        [] ->
                            List.reverse z.leftReversed
    in
    \ctx forest ->
        build
            forest
            { leftReversed = []
            , context = ctx
            , crumbs = []
            }


transformForest : (a -> List tree -> tree) -> Forest a -> List tree
transformForest render =
    transformForestWithContext
        { transform = \a () -> render a
        , childContext = \_ _ -> ()
        }
        ()


debug =
    --    True
    False



-- TREE


type Tree a
    = Tree a (List (Tree a))


mapTreeData : (a -> a) -> Tree a -> Tree a
mapTreeData func (Tree a children) =
    Tree (func a) children


treeData : Tree a -> a
treeData (Tree a _) =
    a


treeChildren : Tree a -> Forest a
treeChildren (Tree _ children) =
    children


leaf : a -> Tree a
leaf a =
    Tree a []



-- FOREST


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


fromSingletonForest : Tree a -> ForestZipper a
fromSingletonForest tree =
    { leftReversed = [], center = tree, right_ = [], crumbs = [] }


fromForest : Forest a -> Maybe (ForestZipper a)
fromForest forest =
    case forest of
        [] ->
            Nothing

        first :: rest ->
            Just { leftReversed = [], center = first, right_ = rest, crumbs = [] }


toRootForest : ForestZipper a -> Forest a
toRootForest =
    firstRoot >> getForest


getForest : ForestZipper a -> Forest a
getForest fz =
    List.reverse fz.leftReversed ++ fz.center :: fz.right_


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


isFirst : ForestZipper a -> Bool
isFirst fz =
    List.isEmpty fz.leftReversed


isLast : ForestZipper a -> Bool
isLast fz =
    List.isEmpty fz.right_



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



-- NAVIGATION HELPER


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
                case firstOf [ down, right, nextSiblingOfClosestAncestor ] acc of
                    Just nextAcc ->
                        findFromCurrent pred nextAcc

                    Nothing ->
                        Nothing


firstOf : List (a -> Maybe b) -> a -> Maybe b
firstOf funcList a =
    case funcList of
        [] ->
            Nothing

        firstFunc :: rest ->
            case firstFunc a of
                Just b ->
                    Just b

                Nothing ->
                    firstOf rest a


findFirst : (a -> Bool) -> ForestZipper a -> Maybe (ForestZipper a)
findFirst pred acc =
    firstRoot acc |> findFromCurrent pred



-- VISIT
--
--
--fzVisit :
--    { enter : ForestZipper a -> acc -> acc
--    , exit : ForestZipper a -> acc -> acc
--    }
--    -> acc
--    -> ForestZipper a
--    -> acc
--fzVisit { enter, exit } =
--    let
--        step : VisitMsg -> acc -> ForestZipper a -> acc
--        step msg acc oz =
--            case msg of
--                Enter ->
--                    step Entered (enter oz acc) oz
--
--                Entered ->
--                    case down oz of
--                        Just childOZ ->
--                            step Enter acc childOZ
--
--                        Nothing ->
--                            step Exit acc oz
--
--                Exit ->
--                    step Exited (exit oz acc) oz
--
--                Exited ->
--                    case right oz of
--                        Just rightOZ ->
--                            step Enter acc rightOZ
--
--                        Nothing ->
--                            step Up acc oz
--
--                Up ->
--                    case up oz of
--                        Just parentOZ ->
--                            step Exit acc parentOZ
--
--                        Nothing ->
--                            acc
--
--        enterFirstRoot : acc -> ForestZipper a -> acc
--        enterFirstRoot acc fz =
--            step Enter acc (firstRoot fz)
--    in
--    enterFirstRoot
--
--
--type VisitMsg
--    = Enter
--    | Entered
--    | Exit
--    | Exited
--    | Up
-- INSERTION


insertLeft : Tree a -> ForestZipper a -> ForestZipper a
insertLeft tree acc =
    { acc | leftReversed = tree :: acc.leftReversed }


insertAndGoRight : Tree a -> ForestZipper a -> ForestZipper a
insertAndGoRight =
    let
        insertRight : Tree a -> ForestZipper a -> ForestZipper a
        insertRight tree acc =
            { acc | right_ = tree :: acc.right_ }

        insertHelp : Tree a -> ForestZipper a -> Maybe (ForestZipper a)
        insertHelp tree =
            insertRight tree >> right
    in
    withRollback << insertHelp


prependAndGotoChild : Tree a -> ForestZipper a -> ForestZipper a
prependAndGotoChild child acc =
    case acc.center of
        Tree a children ->
            { acc
                | center = child
                , leftReversed = []
                , right_ = children
                , crumbs =
                    { leftReversed = acc.leftReversed, datum = a, right_ = acc.right_ }
                        :: acc.crumbs
            }


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
