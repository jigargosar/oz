module Forest.Zipper exposing
    ( Crumb
    , ForestZipper
    , Location(..)
    , ancestors
    , appendChild
    , appendChildGo
    , childrenAsZipper
    , data
    , decoder
    , down
    , encoder
    , findFirst
    , firstRoot
    , fromForest
    , fromTree
    , insertAndGoto
    , insertLeft
    , insertLeftGo
    , insertRight
    , insertRightGo
    , isLeaf
    , lastChild
    , left
    , mapAncestors
    , mapData
    , mapTree
    , mergeChild
    , prependChild
    , prependChildGo
    , remove
    , restructure
    , right
    , rootForest
    , transferAllLevelsFrom
    , transferOneLevelForm
    , tree
    , treeAsZipper
    , up
    )

import Forest.Tree as Tree exposing (Forest, Tree)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Utils exposing (..)


type Location
    = Before
    | After
    | PrependChild
    | AppendChild


insertAndGoto : Location -> Tree a -> ForestZipper a -> ForestZipper a
insertAndGoto location =
    case location of
        Before ->
            insertLeftGo

        After ->
            insertRightGo

        PrependChild ->
            prependChildGo

        AppendChild ->
            appendChildGo



-- FOREST ZIPPER MODEL


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


fromTree : Tree a -> ForestZipper a
fromTree t =
    { leftReversed = [], center = t, right_ = [], crumbs = [] }


fromForest : Forest a -> Maybe (ForestZipper a)
fromForest trees =
    case trees of
        [] ->
            Nothing

        first :: rest ->
            Just { leftReversed = [], center = first, right_ = rest, crumbs = [] }


forest : ForestZipper a -> Forest a
forest fz =
    List.reverse fz.leftReversed ++ fz.center :: fz.right_


rootForest : ForestZipper a -> Forest a
rootForest =
    firstRoot >> forest


childrenAsZipper : ForestZipper a -> Maybe (ForestZipper a)
childrenAsZipper =
    tree >> Tree.children >> fromForest


treeAsZipper : ForestZipper a -> ForestZipper a
treeAsZipper =
    tree >> fromTree


mergeChild : ForestZipper a -> ForestZipper a -> ForestZipper a
mergeChild cz zipper =
    let
        ret =
            mergeInternal cz zipper

        _ =
            if rootForest ret == rootForest (replaceChildrenWithZipper cz zipper) then
                "All is well"

            else
                Debug.todo "invalid merge"
    in
    ret


transferAllLevelsFrom : ForestZipper a -> ForestZipper a -> ForestZipper a
transferAllLevelsFrom parent child =
    mergeChild child parent


mergeInternal : ForestZipper a -> ForestZipper a -> ForestZipper a
mergeInternal cz zipper =
    { zipper
        | leftReversed = cz.leftReversed
        , center = cz.center
        , right_ = cz.right_
        , crumbs =
            cz.crumbs
                ++ { leftReversed = zipper.leftReversed
                   , datum = Tree.data zipper.center
                   , right_ = zipper.right_
                   }
                :: zipper.crumbs
    }


transferOneLevelTo : ForestZipper a -> ForestZipper a -> ( ForestZipper a, Maybe (ForestZipper a) )
transferOneLevelTo cz zipper =
    let
        ret =
            transferOneLevelToInternal cz zipper

        _ =
            if rootForest (mergeInternal cz zipper) == rootForest (transferOneLevelLoop ret) then
                "All is well"

            else
                Debug.todo "invalid merge"
    in
    ret


transferOneLevelForm zipper cz =
    transferOneLevelTo cz zipper |> swap


transferOneLevelLoop : ( ForestZipper a, Maybe (ForestZipper a) ) -> ForestZipper a
transferOneLevelLoop ( cz, maybeZipper ) =
    case maybeZipper of
        Nothing ->
            cz

        Just zipper ->
            transferOneLevelLoop (transferOneLevelToInternal cz zipper)


transferOneLevelToInternal : ForestZipper a -> ForestZipper a -> ( ForestZipper a, Maybe (ForestZipper a) )
transferOneLevelToInternal cz zipper =
    let
        newChildZipper =
            { cz
                | crumbs = cz.crumbs ++ [ { leftReversed = zipper.leftReversed, datum = Tree.data zipper.center, right_ = zipper.right_ } ]
            }
    in
    case zipper.crumbs of
        [] ->
            ( newChildZipper
            , Nothing
            )

        first :: rest ->
            ( newChildZipper
            , Just
                { zipper
                    | leftReversed = first.leftReversed
                    , center = Tree.tree first.datum (List.reverse zipper.leftReversed ++ zipper.center :: zipper.right_)
                    , right_ = first.right_
                    , crumbs = rest
                }
            )


replaceChildrenWithZipper : ForestZipper a -> ForestZipper a -> ForestZipper a
replaceChildrenWithZipper newFiz fiz =
    { fiz | center = Tree.mapChildren (always (rootForest newFiz)) fiz.center }


mapTree : (Tree a -> Tree a) -> ForestZipper a -> ForestZipper a
mapTree func fz =
    { fz | center = func fz.center }


tree : ForestZipper a -> Tree a
tree fz =
    fz.center


mapCrumbData : (a -> a) -> Crumb a -> Crumb a
mapCrumbData func crumb =
    { crumb | datum = func crumb.datum }


mapAncestors : (a -> a) -> ForestZipper a -> ForestZipper a
mapAncestors func fz =
    { fz | crumbs = List.map (mapCrumbData func) fz.crumbs }


ancestors : ForestZipper a -> List a
ancestors =
    .crumbs >> List.map .datum


encoder : (a -> Value) -> ForestZipper a -> Value
encoder aEncoder zipper =
    let
        treeEncoder tre =
            JE.object
                [ ( "item", aEncoder (Tree.data tre) )
                , ( "children", JE.list treeEncoder (Tree.children tre) )
                ]

        zCrumbEncoder crumb =
            JE.object
                [ ( "leftReversed", JE.list treeEncoder crumb.leftReversed )
                , ( "datum", aEncoder crumb.datum )
                , ( "right_", JE.list treeEncoder crumb.right_ )
                ]
    in
    JE.object
        [ ( "leftReversed", JE.list treeEncoder zipper.leftReversed )
        , ( "center", treeEncoder zipper.center )
        , ( "right_", JE.list treeEncoder zipper.right_ )
        , ( "crumbs", JE.list zCrumbEncoder zipper.crumbs )
        ]


decoder : Decoder a -> Decoder (ForestZipper a)
decoder dataDecoder =
    let
        td : Decoder (Tree a)
        td =
            treeDecoder dataDecoder

        cd : Decoder (Crumb a)
        cd =
            JD.succeed Crumb
                |> required "leftReversed" (JD.list td)
                |> required "datum" dataDecoder
                |> required "right_" (JD.list td)
    in
    JD.succeed ForestZipper
        |> required "leftReversed" (JD.list td)
        |> required "center" td
        |> required "right_" (JD.list td)
        |> required "crumbs" (JD.list cd)


required : String -> Decoder a -> Decoder (a -> b) -> Decoder b
required fieldName decoder_ =
    JD.map2 (|>) (JD.field fieldName decoder_)


treeDecoder : Decoder a -> Decoder (Tree a)
treeDecoder dataDecoder =
    JD.succeed Tree.tree
        |> required "item" dataDecoder
        |> required "children" (JD.list (JD.lazy (\_ -> treeDecoder dataDecoder)))



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
                    , center = Tree.tree datum (List.reverse acc.leftReversed ++ acc.center :: acc.right_)
                    , right_ = right_
                    , crumbs = rest
                }


down : ForestZipper a -> Maybe (ForestZipper a)
down acc =
    case ( Tree.data acc.center, Tree.children acc.center ) of
        ( _, [] ) ->
            Nothing

        ( a, first :: rest ) ->
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



-- INSERTION


insertLeft : Tree a -> ForestZipper a -> ForestZipper a
insertLeft node acc =
    { acc | leftReversed = node :: acc.leftReversed }


insertRight : Tree a -> ForestZipper a -> ForestZipper a
insertRight node acc =
    { acc | right_ = node :: acc.right_ }



-- DELETION


remove : ForestZipper a -> Maybe (ForestZipper a)
remove fz =
    let
        leaf : a -> Tree a
        leaf a =
            Tree.tree a []
    in
    case ( fz.leftReversed, fz.right_ ) of
        -- center is the only child; go up
        ( [], [] ) ->
            case fz.crumbs of
                -- center is the only tree in entire forest; cannot remove
                [] ->
                    Nothing

                { leftReversed, datum, right_ } :: rest ->
                    Just
                        { fz
                            | leftReversed = leftReversed
                            , center = leaf datum
                            , right_ = right_
                            , crumbs = rest
                        }

        -- has right siblings; go right
        ( _, first :: rest ) ->
            Just { fz | center = first, right_ = rest }

        -- has left siblings; go left
        ( first :: rest, _ ) ->
            Just { fz | leftReversed = rest, center = first }



-- NAVIGATION HELPER


applyWhileJust : (a -> Maybe a) -> a -> a
applyWhileJust func a =
    case func a of
        Just a2 ->
            applyWhileJust func a2

        Nothing ->
            a


firstRoot : ForestZipper a -> ForestZipper a
firstRoot =
    let
        root : ForestZipper a -> ForestZipper a
        root =
            applyWhileJust up

        firstSibling : ForestZipper a -> ForestZipper a
        firstSibling =
            applyWhileJust left
    in
    root >> firstSibling



-- EXTRA HELPER FUNCTIONS


mapData : (a -> a) -> ForestZipper a -> ForestZipper a
mapData func =
    mapTree (Tree.mapData func)


data : ForestZipper a -> a
data =
    tree >> Tree.data


prependChild : Tree a -> ForestZipper a -> ForestZipper a
prependChild child =
    mapTree (Tree.mapChildren ((::) child))


appendChild : Tree a -> ForestZipper a -> ForestZipper a
appendChild child =
    mapTree (Tree.mapChildren (\children -> children ++ [ child ]))


isLeaf : ForestZipper a -> Bool
isLeaf =
    tree >> Tree.children >> List.isEmpty


lastChild : ForestZipper a -> Maybe (ForestZipper a)
lastChild =
    down >> Maybe.map (applyWhileJust right)


insertRightGo : Tree a -> ForestZipper a -> ForestZipper a
insertRightGo =
    insertAndGo insertRight right


insertLeftGo : Tree a -> ForestZipper a -> ForestZipper a
insertLeftGo =
    insertAndGo insertLeft left


appendChildGo : Tree a -> ForestZipper a -> ForestZipper a
appendChildGo =
    insertAndGo appendChild lastChild


prependChildGo : Tree a -> ForestZipper a -> ForestZipper a
prependChildGo =
    insertAndGo prependChild down


insertAndGo :
    (Tree a -> ForestZipper a -> ForestZipper a)
    -> (ForestZipper a -> Maybe (ForestZipper a))
    -> Tree a
    -> ForestZipper a
    -> ForestZipper a
insertAndGo insertFunc focusFunc node zipper =
    insertFunc node zipper
        |> focusFunc
        |> Maybe.withDefault zipper



-- FIND


find : (a -> Bool) -> (ForestZipper a -> Maybe (ForestZipper a)) -> ForestZipper a -> Maybe (ForestZipper a)
find pred =
    findWithIterator (data >> pred)


findFirst : (a -> Bool) -> ForestZipper a -> Maybe (ForestZipper a)
findFirst pred =
    firstRoot >> find pred (firstOf [ down, right, nextSiblingOfAncestor ])


nextSiblingOfAncestor : ForestZipper a -> Maybe (ForestZipper a)
nextSiblingOfAncestor z =
    case up z of
        Just parentZ ->
            case right parentZ of
                Just ns ->
                    Just ns

                Nothing ->
                    nextSiblingOfAncestor parentZ

        Nothing ->
            Nothing



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


restructure : (ForestZipper a -> List b -> b) -> ForestZipper a -> List b
restructure render zipper =
    restructureHelp render ReGoDown { leftReversed = [], crumbs = [], fiz = firstRoot zipper }


type RestructureMsg
    = ReGoDown
    | ReGoRight
    | ReGoUp


type alias ReAcc a b =
    { leftReversed : List b, crumbs : List (List b), fiz : ForestZipper a }


restructureHelp : (ForestZipper a -> List b -> b) -> RestructureMsg -> ReAcc a b -> List b
restructureHelp render msg acc =
    case msg of
        ReGoDown ->
            case down acc.fiz of
                Just fiz ->
                    restructureHelp render
                        ReGoDown
                        { acc
                            | leftReversed = []
                            , crumbs = acc.leftReversed :: acc.crumbs
                            , fiz = fiz
                        }

                Nothing ->
                    restructureHelp render
                        ReGoRight
                        { acc
                            | leftReversed = render acc.fiz [] :: acc.leftReversed
                        }

        ReGoRight ->
            case right acc.fiz of
                Just fiz ->
                    restructureHelp render
                        ReGoDown
                        { acc | fiz = fiz }

                Nothing ->
                    restructureHelp render
                        ReGoUp
                        acc

        ReGoUp ->
            case ( up acc.fiz, acc.crumbs ) of
                ( Nothing, [] ) ->
                    List.reverse acc.leftReversed

                ( Just fiz, first :: rest ) ->
                    restructureHelp render
                        ReGoRight
                        { acc
                            | leftReversed = render fiz (List.reverse acc.leftReversed) :: first
                            , crumbs = rest
                            , fiz = fiz
                        }

                ( Nothing, _ :: _ ) ->
                    Debug.todo "impl"

                ( Just _, [] ) ->
                    Debug.todo "impl"
