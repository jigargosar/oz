port module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, div, text)
import Html.Attributes exposing (attribute, class, draggable, style)
import Html.Events as Event exposing (onClick)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Maybe.Extra
import Random exposing (Generator)
import Random.Extra


port getBeacons : () -> Cmd msg


port gotBeacons : (Value -> msg) -> Sub msg


port saveOZ : Value -> Cmd msg



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { outline : Outline
    }


type Outline
    = EmptyOutline
    | Outline OZ
    | OutlineDnD Dnd OZ
    | OutlineEdit OZ String


type alias Item =
    { id : ItemId
    , title : String
    }


type alias OutlineNode =
    Tree Item


type alias OZ =
    ForestZipper Item


outlineZipperEncoder : OZ -> Value
outlineZipperEncoder outlineZipper =
    JE.object
        [ ( "leftReversed", JE.list itemTreeEncoder outlineZipper.leftReversed )
        , ( "center", itemTreeEncoder outlineZipper.center )
        , ( "right_", JE.list itemTreeEncoder outlineZipper.right_ )
        , ( "crumbs", JE.list crumbEncoder outlineZipper.crumbs )
        ]


itemTreeEncoder : Tree Item -> Value
itemTreeEncoder (Tree item children) =
    JE.object
        [ ( "item", itemEncoder item )
        , ( "children", JE.list itemTreeEncoder children )
        ]


itemEncoder : Item -> Value
itemEncoder item =
    JE.object
        [ ( "id", itemIdEncoder item.id )
        , ( "title", JE.string item.title )
        ]


crumbEncoder : Crumb Item -> Value
crumbEncoder crumb =
    JE.object
        [ ( "leftReversed", JE.list itemTreeEncoder crumb.leftReversed )
        , ( "datum", itemEncoder crumb.datum )
        , ( "right_", JE.list itemTreeEncoder crumb.right_ )
        ]


required fieldName decoder =
    JD.map2 (|>) (JD.field fieldName decoder)


outlineZipperDecoder : Decoder OZ
outlineZipperDecoder =
    JD.succeed ForestZipper
        |> required "leftReversed" (JD.list treeDecoder)
        |> required "center" treeDecoder
        |> required "right_" (JD.list treeDecoder)
        |> required "crumbs" (JD.list crumbDecoder)


treeDecoder : Decoder OutlineNode
treeDecoder =
    JD.succeed Tree
        |> required "item" itemDecoder
        |> required "children" (JD.list (JD.lazy (\_ -> treeDecoder)))


itemDecoder : Decoder Item
itemDecoder =
    JD.succeed Item
        |> required "id" itemIdDecoder
        |> required "title" JD.string


crumbDecoder : Decoder (Crumb Item)
crumbDecoder =
    JD.succeed Crumb
        |> required "leftReversed" (JD.list treeDecoder)
        |> required "datum" itemDecoder
        |> required "right_" (JD.list treeDecoder)



-- OUTLINE DRAG AND DROP


type alias Dnd =
    { dragItemId : ItemId
    , clientXY : XY
    , offsetXY : XY
    }



-- DRAG AND DROP BEACON: HELPS DETERMINE DROP INTENT


type alias Beacon =
    ( CandidateLocation, Rect )



-- CANDIDATE LOCATION


type CandidateLocation
    = Before ItemId
    | After ItemId
    | PrependIn ItemId
    | AppendIn ItemId



-- MODEL


type alias Flags =
    { oz : Value }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initialItemGenerator : Generator (List Item)
        initialItemGenerator =
            [ "Quick Brown Fox Jumped Over The Lazy Dog"
            , "Take Notes"
            , "Thou shall not experiment with experiments"
            , "Watch Movies"
            , "Run the mill"
            ]
                |> List.map itemGenerator
                |> Random.Extra.combine

        seed0 =
            Random.initialSeed 0

        ( initialItems, _ ) =
            Random.step initialItemGenerator seed0

        outline =
            initialItems |> List.map (\item -> Tree item [])

        oz =
            case JD.decodeValue (JD.nullable outlineZipperDecoder) flags.oz of
                Ok got ->
                    case got of
                        Nothing ->
                            fromForest outline

                        Just oz_ ->
                            Just oz_

                Err err ->
                    Debug.log "oz" (JD.errorToString err)
                        |> always Nothing
    in
    ( { outline = Maybe.map Outline oz |> Maybe.withDefault EmptyOutline
      }
    , Cmd.none
    )



-- ITEM HELPERS


type ItemId
    = ItemId String


itemGenerator : String -> Generator Item
itemGenerator title =
    itemIdGen
        |> Random.map (\id -> { id = id, title = title })


itemIdGen : Generator ItemId
itemIdGen =
    Random.int 10000 Random.maxInt
        |> Random.map (String.fromInt >> (++) "item-id-" >> ItemId)


itemIdEncoder : ItemId -> Value
itemIdEncoder (ItemId string) =
    JE.string string


itemIdDecoder : Decoder ItemId
itemIdDecoder =
    JD.string
        |> JD.andThen
            (\idStr ->
                if String.startsWith "item-id-" idStr then
                    JD.succeed (ItemId idStr)

                else
                    JD.fail ("invalid item id prefix: " ++ idStr)
            )



-- DND HELPERS


dndDraggedXY : Dnd -> XY
dndDraggedXY dnd =
    subtractXY dnd.clientXY dnd.offsetXY


dndClosestCandidateLocation : List Beacon -> Dnd -> Maybe CandidateLocation
dndClosestCandidateLocation beacons dnd =
    let
        draggedXY : XY
        draggedXY =
            dndDraggedXY dnd

        sortByFunc : Beacon -> Float
        sortByFunc ( _, rect ) =
            distanceFromRectCenterTo draggedXY rect
    in
    beacons
        |> List.sortBy sortByFunc
        |> List.head
        |> Maybe.map Tuple.first


beaconDecoder : Decoder Beacon
beaconDecoder =
    JD.map2 Tuple.pair
        (JD.field "id" candidateLocationDecoder)
        rectDecoder


candidateLocationEncoder : CandidateLocation -> Value
candidateLocationEncoder candidateLocation =
    let
        encodeHelp : String -> ItemId -> Value
        encodeHelp tagName itemId =
            JE.object
                [ ( "tag", JE.string tagName )
                , ( "id", itemIdEncoder itemId )
                ]
    in
    case candidateLocation of
        Before itemId ->
            encodeHelp "Before" itemId

        After itemId ->
            encodeHelp "After" itemId

        PrependIn itemId ->
            encodeHelp "PrependIn" itemId

        AppendIn itemId ->
            encodeHelp "AppendIn" itemId


candidateLocationDecoder : Decoder CandidateLocation
candidateLocationDecoder =
    let
        decodeHelp : (ItemId -> CandidateLocation) -> Decoder CandidateLocation
        decodeHelp tag =
            JD.field "id" itemIdDecoder
                |> JD.map tag

        tagDecoder : String -> Decoder CandidateLocation
        tagDecoder tag =
            case tag of
                "Before" ->
                    decodeHelp Before

                "After" ->
                    decodeHelp After

                "PrependIn" ->
                    decodeHelp PrependIn

                "AppendIn" ->
                    decodeHelp AppendIn

                _ ->
                    JD.fail ("unknown tag for CandidateLocation: " ++ tag)
    in
    JD.field "tag" JD.string |> JD.andThen tagDecoder



-- Update


type Msg
    = NoOp
    | Start Dnd
    | Move XY
    | Stop
    | GotBeacons Value
    | ItemTitleClicked ItemId


cacheOZCmd : OZ -> Cmd msg
cacheOZCmd =
    outlineZipperEncoder >> saveOZ


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        ItemTitleClicked iid ->
            case model.outline of
                EmptyOutline ->
                    Debug.todo "impl"

                Outline oz ->
                    case gotoNodeWithId iid oz of
                        Just noz ->
                            ( { model | outline = OutlineEdit noz (ozTitle noz) }, cacheOZCmd noz )

                        Nothing ->
                            ( model, Cmd.none )

                OutlineDnD _ _ ->
                    Debug.todo "impl"

                OutlineEdit oz title ->
                    let
                        noz =
                            ozSetTitle title oz
                                |> withRollback (gotoNodeWithId iid)
                    in
                    ( { model | outline = Outline noz }, cacheOZCmd noz )

        Start dnd ->
            case model.outline of
                EmptyOutline ->
                    Debug.todo "impl"

                Outline oz ->
                    ( { model | outline = OutlineDnD dnd oz }, getBeacons () )

                OutlineDnD _ _ ->
                    Debug.todo "impl"

                OutlineEdit _ _ ->
                    Debug.todo "impl"

        Move clientXY ->
            case model.outline of
                EmptyOutline ->
                    Debug.todo "impl"

                Outline _ ->
                    Debug.todo "impl"

                OutlineDnD dnd oz ->
                    ( { model | outline = OutlineDnD { dnd | clientXY = clientXY } oz }, getBeacons () )

                OutlineEdit _ _ ->
                    Debug.todo "impl"

        Stop ->
            case model.outline of
                EmptyOutline ->
                    Debug.todo "impl"

                Outline _ ->
                    Debug.todo "impl"

                OutlineDnD _ oz ->
                    ( { model | outline = Outline oz }, Cmd.none )

                OutlineEdit _ _ ->
                    Debug.todo "impl"

        GotBeacons encodedBeacons ->
            case model.outline of
                EmptyOutline ->
                    Debug.todo "impl"

                Outline _ ->
                    Debug.todo "impl"

                OutlineDnD dnd oz ->
                    let
                        beaconsResult =
                            JD.decodeValue (JD.list beaconDecoder) encodedBeacons

                        maybeNoz =
                            Result.toMaybe beaconsResult
                                |> Maybe.andThen
                                    (\beacons -> dndClosestCandidateLocation beacons dnd)
                                |> Maybe.andThen
                                    (\cl -> moveItemWithIdToCandidateLocation dnd.dragItemId cl oz)
                                |> Maybe.andThen (gotoNodeWithId dnd.dragItemId)
                    in
                    case maybeNoz of
                        Just noz ->
                            ( { model | outline = OutlineDnD dnd noz }, cacheOZCmd noz )

                        Nothing ->
                            ( model, Cmd.none )

                OutlineEdit _ _ ->
                    Debug.todo "impl"


ozTitle : OZ -> String
ozTitle =
    ozItem >> .title


ozItem : OZ -> Item
ozItem =
    getTree >> treeData


ozId : OZ -> ItemId
ozId =
    ozItem >> .id


ozSetTitle : String -> OZ -> OZ
ozSetTitle title =
    fzMapData (\item -> { item | title = title })


gotoNodeWithId : ItemId -> OZ -> Maybe OZ
gotoNodeWithId itemId =
    findFirst (propEq .id itemId)


moveItemWithIdToCandidateLocation : ItemId -> CandidateLocation -> OZ -> Maybe OZ
moveItemWithIdToCandidateLocation srcItemId candidateLocation =
    let
        moveTo : CandidateLocation -> OZ -> Maybe OZ
        moveTo atLocation zipper =
            remove zipper
                |> Maybe.andThen (insertRemovedNodeAtLocation atLocation zipper.center)

        insertRemovedNodeAtLocation : CandidateLocation -> OutlineNode -> OZ -> Maybe OZ
        insertRemovedNodeAtLocation atLocation node =
            let
                insertHelp targetItemId func =
                    gotoNodeWithId targetItemId
                        >> Maybe.map (func node)
            in
            case atLocation of
                Before itemId ->
                    insertHelp itemId insertLeft

                After itemId ->
                    insertHelp itemId insertRight

                PrependIn itemId ->
                    insertHelp itemId insertFirstChild

                AppendIn itemId ->
                    insertHelp itemId insertLastChild
    in
    gotoNodeWithId srcItemId
        >> Maybe.andThen (moveTo candidateLocation)


subscriptions : Model -> Sub Msg
subscriptions m =
    Sub.batch
        [ case m.outline of
            EmptyOutline ->
                Sub.none

            Outline _ ->
                Sub.none

            OutlineDnD _ _ ->
                Sub.batch
                    [ Browser.Events.onMouseMove (JD.map Move clientXYDecoder)
                    , Browser.Events.onMouseUp (JD.succeed Stop)
                    , gotBeacons GotBeacons
                    ]

            OutlineEdit _ _ ->
                Sub.none
        ]



-- View


view : Model -> Html Msg
view m =
    div [ class "pv3 ph5 measure-narrow f3 lh-copy" ]
        [ div [ class "pv2" ] [ text "DND Beacons Ports" ]
        , viewOutline m.outline
        ]


viewOutline : Outline -> Html Msg
viewOutline outline =
    case outline of
        EmptyOutline ->
            text "IMPLEMENT : EMPTY OUTLINE VIEW"

        Outline oz ->
            let
                vh =
                    viewItemTree
                        { dragId = Nothing
                        , focusedId = Just (ozId oz)
                        }
            in
            div [] (List.map vh (toForest oz))

        OutlineDnD dnd oz ->
            let
                maybeDraggedItemId =
                    Just dnd.dragItemId

                info =
                    { dragId = maybeDraggedItemId
                    , focusedId = Just (ozId oz)
                    }

                viewHelp =
                    viewItemTree info
            in
            div [] (List.map viewHelp (toForest oz))

        OutlineEdit oz _ ->
            let
                vh =
                    viewItemTree
                        { dragId = Nothing
                        , focusedId = Just (ozId oz)
                        }
            in
            div [] (List.map vh (toForest oz))


type FlatLine
    = BeaconLine Int CandidateLocation
    | ItemLine Int Item { isHighlighted : Bool, isDraggable : Bool }
    | EditItemLine


fzFoldlHelp : (ForestZipper a -> acc -> acc) -> ForestZipper a -> acc -> acc
fzFoldlHelp func fz acc =
    let
        acc2 : acc
        acc2 =
            func fz acc
    in
    case Maybe.Extra.oneOf [ down, right, nextSiblingOfClosestAncestor ] fz of
        Just nfz ->
            fzFoldlHelp func nfz acc2

        Nothing ->
            acc


fzFoldl : (ForestZipper a -> acc -> acc) -> ForestZipper a -> acc -> acc
fzFoldl func fz =
    fzFoldlHelp func (firstRoot fz)


ozToFlatLines : ItemId -> OZ -> List FlatLine
ozToFlatLines highlightedId =
    let
        func : OZ -> List FlatLine -> List FlatLine
        func oz acc =
            let
                ( level, item ) =
                    ( getLevel oz, ozItem oz )

                itemLine : FlatLine
                itemLine =
                    ItemLine level item { isHighlighted = highlightedId == item.id, isDraggable = True }

                appendInParent parentItemId =
                    BeaconLine (level - 1) (AppendIn parentItemId)

                maybeAppendInBeaconLine : Maybe FlatLine
                maybeAppendInBeaconLine =
                    case right oz of
                        Nothing ->
                            up oz |> Maybe.map (ozId >> appendInParent)

                        Just _ ->
                            Nothing
            in
            acc
                ++ ([ BeaconLine level (Before item.id)
                    , itemLine
                    , BeaconLine level (After item.id)
                    , BeaconLine (level + 1) (PrependIn item.id)
                    ]
                        ++ Maybe.Extra.toList maybeAppendInBeaconLine
                   )
    in
    \oz -> fzFoldl func oz []


toFlatLines : Outline -> List FlatLine
toFlatLines outline =
    case outline of
        EmptyOutline ->
            []

        Outline oz ->
            let
                highlightedItemId =
                    ozId oz
            in
            ozToFlatLines highlightedItemId oz

        OutlineDnD dnd oz ->
            Debug.todo "impl"

        OutlineEdit oz string ->
            Debug.todo "impl"


viewFlatLine : FlatLine -> Html Msg
viewFlatLine flatLine =
    case flatLine of
        BeaconLine level candidateLocation ->
            div [ style "padding-left" (String.fromInt (level * 32) ++ "px") ]
                [ viewBeacon candidateLocation
                ]

        ItemLine level item { isHighlighted, isDraggable } ->
            div [ style "padding-left" (String.fromInt (level * 32) ++ "px") ]
                [ div
                    (class "pa1 bb b--black-10 pointer no-selection"
                        :: (if isHighlighted then
                                class "bg-blue white"

                            else
                                class ""
                           )
                        :: onClick (ItemTitleClicked item.id)
                        :: (if isDraggable then
                                dragEvents item.id

                            else
                                []
                           )
                    )
                    [ div [ class "lh-title" ] [ text item.title ] ]
                ]

        EditItemLine ->
            text ""


type alias ViewInfo =
    { dragId : Maybe ItemId
    , focusedId : Maybe ItemId
    }


viewItemTree : ViewInfo -> OutlineNode -> Html Msg
viewItemTree info (Tree item children) =
    if info.dragId == Just item.id then
        div [ class "o-50" ] [ viewItemTreeWithoutBeacons (Tree item children) ]

    else
        div
            []
            [ viewBeacon (Before item.id)
            , viewItemTitle (info.focusedId == Just item.id) item
            , let
                itemId =
                    item.id
              in
              childrenContainer
                (viewBeacon (PrependIn itemId)
                    :: List.map (viewItemTree info) children
                    ++ [ viewBeacon (AppendIn itemId) ]
                )
            , viewBeacon (After item.id)
            ]


viewBeacon : CandidateLocation -> Html msg
viewBeacon cl =
    div
        [ style "height" "1px"
        , style "width" "1px"
        , class "bg-blue"
        , class "absolute"
        , attribute "data-beacon" (JE.encode 0 (candidateLocationEncoder cl))
        ]
        []


viewItemTreeWithoutBeacons : OutlineNode -> Html Msg
viewItemTreeWithoutBeacons (Tree item children) =
    div []
        [ viewItemTitle False item
        , childrenContainer
            (List.map viewItemTreeWithoutBeacons children)
        ]


viewItemTitle : Bool -> Item -> Html Msg
viewItemTitle isSelected item =
    div
        (class "pa1 bb b--black-10 pointer no-selection"
            :: (if isSelected then
                    class "bg-blue white"

                else
                    class ""
               )
            :: onClick (ItemTitleClicked item.id)
            :: dragEvents item.id
        )
        [ div [ class "lh-title" ] [ text item.title ] ]


dragEvents : ItemId -> List (Html.Attribute Msg)
dragEvents itemId =
    [ draggable "true"
    , Event.preventDefaultOn "dragstart"
        (JD.map2 (\clientXY offsetXY -> Start (Dnd itemId clientXY offsetXY))
            clientXYDecoder
            offsetXYDecoder
            |> preventDefault True
        )
    ]


preventDefault : Bool -> Decoder b -> Decoder ( b, Bool )
preventDefault bool =
    JD.map (\msg -> ( msg, bool ))


childrenContainer : List (Html Msg) -> Html Msg
childrenContainer =
    div [ class "pl4" ]



-- TREE


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


toForest : ForestZipper a -> Forest a
toForest acc =
    case root acc of
        { leftReversed, center, right_ } ->
            List.reverse leftReversed ++ center :: right_


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



-- XY


type alias XY =
    { x : Float, y : Float }


subtractXY : XY -> XY -> XY
subtractXY a b =
    XY (a.x - b.x) (a.y - b.y)


clientXYDecoder : Decoder XY
clientXYDecoder =
    JD.map2 XY
        (JD.field "clientX" JD.float)
        (JD.field "clientY" JD.float)


offsetXYDecoder : Decoder XY
offsetXYDecoder =
    JD.map2 XY
        (JD.field "offsetX" JD.float)
        (JD.field "offsetY" JD.float)



-- RECT


type alias Rect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


rectDecoder : Decoder Rect
rectDecoder =
    JD.map4 Rect
        (JD.field "x" JD.float)
        (JD.field "y" JD.float)
        (JD.field "width" JD.float)
        (JD.field "height" JD.float)


rectCenter : Rect -> XY
rectCenter rect =
    XY (rect.x + (rect.width / 2)) (rect.y + (rect.height / 2))


distanceFromRectCenterTo : XY -> Rect -> Float
distanceFromRectCenterTo xy rect =
    let
        fromXY =
            rectCenter rect
    in
    sqrt (((xy.x - fromXY.x) ^ 2) + ((xy.y - fromXY.y) ^ 2))
