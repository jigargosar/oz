module Forest.Zipper exposing
    ( Crumb
    , ForestZipper
    , backward
    , decoder
    , down
    , encoder
    , firstRoot
    , forest
    , forward
    , insertLeft
    , insertRight
    , left
    , mapTree
    , remove
    , right
    , tree
    , up
    )

import Forest.Tree as Tree exposing (Forest, Tree)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Maybe.Extra



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



--fromTree : Tree a -> ForestZipper a
--fromTree tree =
--    { leftReversed = [], center = tree, right_ = [], crumbs = [] }
--
--
--fromForest : Forest a -> Maybe (ForestZipper a)
--fromForest forest =
--    case forest of
--        [] ->
--            Nothing
--
--        first :: rest ->
--            Just { leftReversed = [], center = first, right_ = rest, crumbs = [] }


forest : ForestZipper a -> Forest a
forest fz =
    List.reverse fz.leftReversed ++ fz.center :: fz.right_


mapTree : (Tree a -> Tree a) -> ForestZipper a -> ForestZipper a
mapTree func fz =
    { fz | center = func fz.center }


tree : ForestZipper a -> Tree a
tree fz =
    fz.center


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


aTreeDecoder : Decoder a -> Decoder (Tree a)
aTreeDecoder aDecoder =
    JD.succeed Tree.tree
        |> required "item" aDecoder
        |> required "children" (JD.list (JD.lazy (\_ -> aTreeDecoder aDecoder)))


required : String -> Decoder a -> Decoder (a -> b) -> Decoder b
required fieldName decoder_ =
    JD.map2 (|>) (JD.field fieldName decoder_)


decoder : Decoder a -> Decoder (ForestZipper a)
decoder dataDecoder =
    let
        td =
            aTreeDecoder dataDecoder

        zCrumbDecoder =
            JD.succeed Crumb
                |> required "leftReversed" (JD.list td)
                |> required "datum" dataDecoder
                |> required "right_" (JD.list td)
    in
    JD.succeed ForestZipper
        |> required "leftReversed" (JD.list td)
        |> required "center" td
        |> required "right_" (JD.list td)
        |> required "crumbs" (JD.list zCrumbDecoder)



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


backward : ForestZipper a -> Maybe (ForestZipper a)
backward =
    Maybe.Extra.oneOf [ left >> Maybe.map lastDescendant, up ]


forward : ForestZipper a -> Maybe (ForestZipper a)
forward =
    Maybe.Extra.oneOf [ down, right, nextSiblingOfClosestAncestor ]


lastChild : ForestZipper a -> Maybe (ForestZipper a)
lastChild =
    down >> Maybe.map (applyWhileJust right)


lastDescendant : ForestZipper a -> ForestZipper a
lastDescendant zipper =
    case lastChild zipper of
        Nothing ->
            zipper

        Just child ->
            lastDescendant child


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



-- FIND
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