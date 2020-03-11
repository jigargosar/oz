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
    { dnd : Maybe DnD
    , oz : Maybe OZ
    , outline : Outline
    }


type Outline
    = EmptyOutline
    | Outline OZ
    | OutlineDnD DnD OZ
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


type alias DnD =
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
    ( { dnd = Nothing
      , oz = oz
      , outline = Maybe.map Outline oz |> Maybe.withDefault EmptyOutline
      }
    , Cmd.none
    )


mapDnD : (DnD -> DnD) -> Model -> Model
mapDnD func model =
    { model | dnd = Maybe.map func model.dnd }



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


dndDraggedXY : DnD -> XY
dndDraggedXY dnd =
    subtractXY dnd.clientXY dnd.offsetXY


dndClosestCandidateLocation : List Beacon -> DnD -> Maybe CandidateLocation
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
    | Start DnD
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
                    ( model, Cmd.none )

                Outline oz ->
                    let
                        noz =
                            withRollback (gotoNodeWithId iid) oz
                    in
                    ( { model | outline = Outline noz }, cacheOZCmd noz )

                OutlineDnD _ _ ->
                    ( model, Cmd.none )

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

                OutlineDnD dnd_ oz ->
                    Debug.todo "impl"

                OutlineEdit oz string ->
                    Debug.todo "impl"

        Move clientXY ->
            ( mapDnD (\dnd -> { dnd | clientXY = clientXY }) model
            , getBeacons ()
            )

        Stop ->
            ( { model | dnd = Nothing }, Cmd.none )

        GotBeacons encodedBeacons ->
            let
                beaconsResult =
                    JD.decodeValue (JD.list beaconDecoder) encodedBeacons

                updateOutlineWithDnDAndBeacons : DnD -> List Beacon -> Maybe OZ
                updateOutlineWithDnDAndBeacons dnd beacons =
                    Maybe.Extra.andThen2 (moveItemWithIdToCandidateLocation dnd.dragItemId)
                        (dndClosestCandidateLocation beacons dnd)
                        model.oz
                        |> Maybe.andThen (gotoNodeWithId dnd.dragItemId)
            in
            case
                Maybe.Extra.andThen2 updateOutlineWithDnDAndBeacons
                    model.dnd
                    (Result.toMaybe beaconsResult)
            of
                Just oz ->
                    ( { model | oz = Just oz }, cacheOZCmd oz )

                Nothing ->
                    ( model, Cmd.none )



--ozTitle : OZ -> String
--ozTitle =
--    getTree >> treeData >> .title


ozId : OZ -> ItemId
ozId =
    getTree >> treeData >> .id


ozSetTitle : String -> OZ -> OZ
ozSetTitle title =
    mapTree (mapTreeData (\item -> { item | title = title }))


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
        [ Browser.Events.onMouseUp (JD.succeed Stop)
        , case m.dnd of
            Just _ ->
                Browser.Events.onMouseMove (JD.map Move clientXYDecoder)

            Nothing ->
                Sub.none
        , gotBeacons GotBeacons
        ]



-- View


view : Model -> Html Msg
view m =
    div [ class "pv3 ph5 measure-narrow f3 lh-copy" ]
        [ div [ class "pv2" ] [ text "DND Beacons Ports" ]
        , viewOutline m.outline
        , always (text "") <|
            div []
                [ case m.oz |> Maybe.map toForest of
                    Just ol ->
                        let
                            maybeDraggedItemId =
                                m.dnd |> Maybe.map .dragItemId

                            info =
                                { dragId = maybeDraggedItemId
                                , focusedId = m.oz |> Maybe.map (.center >> (\(Tree item _) -> item.id))
                                }

                            viewHelp =
                                viewItemTree info
                        in
                        div [] (List.map viewHelp ol)

                    Nothing ->
                        text ""
                , let
                    dragInfo =
                        Maybe.Extra.andThen2
                            (\dnd ->
                                gotoNodeWithId dnd.dragItemId
                                    >> Maybe.map (.center >> Tuple.pair (dndDraggedXY dnd))
                            )
                            m.dnd
                            m.oz
                  in
                  case dragInfo of
                    Just ( xy, tree ) ->
                        div
                            [ class "no-pe fixed"
                            , style "top" (String.fromFloat xy.y ++ "px")
                            , style "left" (String.fromFloat xy.x ++ "px")
                            ]
                            [ viewItemTreeWithoutBeacons tree ]

                    Nothing ->
                        text ""
                ]
        ]


viewOutline : Outline -> Html Msg
viewOutline outline =
    case outline of
        EmptyOutline ->
            text "EMPTY OUTLINE IMPLEMENT"

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

        OutlineEdit oz string ->
            Debug.todo "viewTODO"


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
        (JD.map2 (\clientXY offsetXY -> Start (DnD itemId clientXY offsetXY))
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


getTree : ForestZipper a -> Tree a
getTree fz =
    fz.center



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
