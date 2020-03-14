port module Main exposing (main)

import Browser
import Browser.Events
import Forest
import Forest.Tree as Tree exposing (Forest, Tree)
import Forest.Zipper as Zipper exposing (ForestZipper)
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (attribute, class, draggable, style, value)
import Html.Events as Event exposing (onClick, onInput)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Random exposing (Generator, Seed)
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
    , seed : Seed
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


type alias OutlineForest =
    List OutlineNode


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
itemTreeEncoder tree =
    JE.object
        [ ( "item", itemEncoder (Tree.data tree) )
        , ( "children", JE.list itemTreeEncoder (Tree.children tree) )
        ]


itemEncoder : Item -> Value
itemEncoder item =
    JE.object
        [ ( "id", itemIdEncoder item.id )
        , ( "title", JE.string item.title )
        ]


crumbEncoder : Zipper.Crumb Item -> Value
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
    JD.succeed Tree.tree
        |> required "item" itemDecoder
        |> required "children" (JD.list (JD.lazy (\_ -> treeDecoder)))


itemDecoder : Decoder Item
itemDecoder =
    JD.succeed Item
        |> required "id" itemIdDecoder
        |> required "title" JD.string


crumbDecoder : Decoder (Zipper.Crumb Item)
crumbDecoder =
    JD.succeed Zipper.Crumb
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
    { oz : Value, now : Int }


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
            Random.initialSeed flags.now

        ( initialItems, seed1 ) =
            Random.step initialItemGenerator seed0

        outline =
            initialItems |> List.map (\item -> Tree.leaf item)

        oz =
            case JD.decodeValue (JD.nullable outlineZipperDecoder) flags.oz of
                Ok got ->
                    case got of
                        Nothing ->
                            Zipper.fromForest outline

                        Just oz_ ->
                            Just oz_

                Err err ->
                    Debug.log "oz" (JD.errorToString err)
                        |> always Nothing
    in
    ( { outline = Maybe.map Outline oz |> Maybe.withDefault EmptyOutline
      , seed = seed1
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
    | TitleChanged String
    | New


cacheOZCmd : OZ -> Cmd msg
cacheOZCmd =
    outlineZipperEncoder >> saveOZ


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        New ->
            case model.outline of
                Outline oz ->
                    let
                        ( newItem, newSeed ) =
                            Random.step (itemGenerator "") model.seed
                    in
                    ( { model
                        | outline = OutlineEdit (ozNew newItem oz) newItem.title
                        , seed = newSeed
                      }
                    , Cmd.none
                    )

                _ ->
                    Debug.todo "impl"

        TitleChanged title ->
            case model.outline of
                OutlineEdit oz _ ->
                    ( { model | outline = OutlineEdit oz title }, Cmd.none )

                _ ->
                    Debug.todo "impl"

        ItemTitleClicked iid ->
            case model.outline of
                EmptyOutline ->
                    Debug.todo "impl"

                Outline oz ->
                    if ozId oz == iid then
                        ( { model | outline = OutlineEdit oz (ozTitle oz) }, cacheOZCmd oz )

                    else
                        case gotoItemId iid oz of
                            Just noz ->
                                ( { model | outline = Outline noz }, cacheOZCmd noz )

                            Nothing ->
                                ( model, Cmd.none )

                OutlineDnD _ _ ->
                    Debug.todo "impl"

                OutlineEdit oz title ->
                    let
                        noz =
                            ozSetTitleUnlessBlankOrRemoveIfBlankLeaf title oz
                                |> withRollback (gotoItemId iid)
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

                OutlineEdit oz title ->
                    let
                        noz =
                            ozSetTitleUnlessBlankOrRemoveIfBlankLeaf title oz
                    in
                    ( { model | outline = OutlineDnD dnd noz }, getBeacons () )

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
                                    (\beacons ->
                                        dndClosestCandidateLocation beacons dnd
                                            |> (if debug then
                                                    Debug.log "debug"

                                                else
                                                    identity
                                               )
                                    )
                                |> Maybe.andThen
                                    (\cl -> moveItemWithIdToCandidateLocation dnd.dragItemId cl oz)
                                |> Maybe.andThen (gotoItemId dnd.dragItemId)
                    in
                    case maybeNoz of
                        Just noz ->
                            ( { model | outline = OutlineDnD dnd noz }, cacheOZCmd noz )

                        Nothing ->
                            ( model, Cmd.none )

                OutlineEdit _ _ ->
                    Debug.todo "impl"


ozSetTitleUnlessBlankOrRemoveIfBlankLeaf : String -> OZ -> OZ
ozSetTitleUnlessBlankOrRemoveIfBlankLeaf title oz =
    if isBlank title then
        if Zipper.isLeaf oz then
            oz
                |> withRollback Zipper.remove

        else
            oz

    else
        ozSetTitle title oz


isBlank : String -> Bool
isBlank =
    String.trim >> String.isEmpty


withRollback func oz =
    func oz |> Maybe.withDefault oz


ozTitle : OZ -> String
ozTitle =
    ozItem >> .title


ozItem : OZ -> Item
ozItem =
    Zipper.data


ozId : OZ -> ItemId
ozId =
    ozItem >> .id


ozNew : Item -> ForestZipper Item -> ForestZipper Item
ozNew item oz =
    Zipper.prependChildAndFocus (Tree.leaf item) oz


ozSetTitle : String -> OZ -> OZ
ozSetTitle title =
    Zipper.mapData (\item -> { item | title = title })


gotoItemId : ItemId -> OZ -> Maybe OZ
gotoItemId itemId =
    Zipper.findFirst (propEq .id itemId)


moveItemWithIdToCandidateLocation : ItemId -> CandidateLocation -> OZ -> Maybe OZ
moveItemWithIdToCandidateLocation srcItemId candidateLocation =
    let
        moveTo : CandidateLocation -> OZ -> Maybe OZ
        moveTo atLocation zipper =
            Zipper.remove zipper
                |> Maybe.andThen (insertRemovedNodeAtLocation atLocation zipper.center)

        insertRemovedNodeAtLocation : CandidateLocation -> OutlineNode -> OZ -> Maybe OZ
        insertRemovedNodeAtLocation atLocation node =
            let
                insertHelp targetItemId func =
                    gotoItemId targetItemId
                        >> Maybe.map (func node)
            in
            case atLocation of
                Before itemId ->
                    insertHelp itemId Zipper.insertLeft

                After itemId ->
                    insertHelp itemId Zipper.insertAndGoRight

                PrependIn itemId ->
                    insertHelp itemId Zipper.prependChildAndFocus

                AppendIn itemId ->
                    insertHelp itemId Zipper.appendChild
    in
    gotoItemId srcItemId
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
        [ viewOutline m.outline
        , viewDraggedNode m.outline
        ]



-- Experimental Outline View


type alias HM =
    Html Msg


type alias LHM =
    List HM


type alias OCtx =
    { renderWithoutBeacons : Bool
    }


viewOutline : Outline -> HM
viewOutline outline =
    div []
        [ div [ class "f1" ] [ text "OZ Outlining" ]
        , div [] (outlineToHtmlList outline)
        ]


outlineToHtmlList : Outline -> LHM
outlineToHtmlList outline =
    case outline of
        EmptyOutline ->
            []

        Outline oz ->
            let
                highlightedId =
                    ozId oz

                forest =
                    Zipper.toRootForest oz
            in
            Forest.restructure identity
                (\item -> renderDraggableWithBeacons (item.id == highlightedId) item)
                forest

        OutlineDnD dnd oz ->
            let
                forest =
                    Zipper.toRootForest oz

                renderForestFns : List (Bool -> HM)
                renderForestFns =
                    Forest.restructure identity
                        (\item renderChildrenFns ->
                            \shouldRenderWithoutBeacon ->
                                let
                                    children bool =
                                        List.map (\f -> f bool) renderChildrenFns
                                in
                                if shouldRenderWithoutBeacon || item.id == dnd.dragItemId then
                                    renderWithoutBeacons item (children True)

                                else
                                    renderNotDraggableWithBeacons item (children False)
                        )
                        forest
            in
            List.map (\fn -> fn False) renderForestFns

        OutlineEdit oz title ->
            let
                editItemId =
                    ozId oz

                renderItem : Item -> LHM -> HM
                renderItem item =
                    if item.id == editItemId then
                        renderEditItem title

                    else
                        renderDraggableWithBeacons False item

                forest =
                    Zipper.toRootForest oz
            in
            Forest.restructure identity renderItem forest


viewDraggedNode : Outline -> Html Msg
viewDraggedNode outline =
    case outline of
        EmptyOutline ->
            text ""

        Outline _ ->
            text ""

        OutlineDnD dnd oz ->
            let
                xy =
                    dndDraggedXY dnd

                draggedForest =
                    gotoItemId dnd.dragItemId oz
                        |> Maybe.map (Zipper.getTree >> List.singleton)
                        |> Maybe.withDefault []
            in
            div
                [ class "fixed no-pe"
                , style "left" (String.fromFloat xy.x ++ "px")
                , style "top" (String.fromFloat xy.y ++ "px")
                ]
                (Forest.restructure identity renderDraggedItem draggedForest)

        OutlineEdit _ _ ->
            text ""


renderWithoutBeacons : Item -> LHM -> HM
renderWithoutBeacons item childrenHtml =
    div [ class "" ]
        [ viewFadedDraggedItem item
        , div [ class "pl4" ] childrenHtml
        ]


renderDraggedItem : Item -> LHM -> HM
renderDraggedItem item childrenHtml =
    div [ class "" ]
        [ viewDraggedItem item
        , div [ class "pl4" ] childrenHtml
        ]


renderNotDraggableWithBeacons : Item -> LHM -> HM
renderNotDraggableWithBeacons item childrenHtml =
    let
        viewBeaconHelp func =
            viewBeacon (func item.id)
    in
    div [ class "" ]
        [ viewBeaconHelp Before
        , viewNotDraggableItem item
        , div [ class "pl4" ] (viewBeaconHelp PrependIn :: childrenHtml ++ [ viewBeaconHelp AppendIn ])
        , viewBeaconHelp After
        ]


renderDraggableWithBeacons : Bool -> Item -> LHM -> HM
renderDraggableWithBeacons isHighlighted item childrenHtml =
    let
        viewBeaconHelp func =
            viewBeacon (func item.id)
    in
    div [ class "" ]
        [ viewBeaconHelp Before
        , viewDraggableItem isHighlighted item
        , div [ class "pl4" ] (viewBeaconHelp PrependIn :: childrenHtml ++ [ viewBeaconHelp AppendIn ])
        , viewBeaconHelp After
        ]


renderEditItem : String -> LHM -> HM
renderEditItem title lhm =
    div [ class "" ]
        [ viewEditItem title
        , div [ class "pl4" ] lhm
        ]


viewBeacon : CandidateLocation -> Html Msg
viewBeacon candidateLocation =
    viewFlatLineWithConfig False (BeaconLine 0 candidateLocation)


viewDraggableItem : Bool -> Item -> Html Msg
viewDraggableItem isHighlighted item =
    viewFlatLineWithConfig False (ItemLine 0 item { isHighlighted = isHighlighted, isDraggable = True })


viewNotDraggableItem : Item -> Html Msg
viewNotDraggableItem item =
    viewFlatLineWithConfig False (ItemLine 0 item { isHighlighted = False, isDraggable = False })


viewFadedDraggedItem : Item -> Html Msg
viewFadedDraggedItem item =
    viewFlatLineWithConfig True (ItemLine 0 item { isHighlighted = False, isDraggable = False })


viewDraggedItem : Item -> Html Msg
viewDraggedItem item =
    viewFlatLineWithConfig False (ItemLine 0 item { isHighlighted = False, isDraggable = False })


viewEditItem : String -> Html Msg
viewEditItem title =
    viewFlatLineWithConfig False (EditLine 0 title)



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



-- FlatLines View


type FlatLine
    = BeaconLine Int CandidateLocation
    | ItemLine Int Item { isHighlighted : Bool, isDraggable : Bool }
    | EditLine Int String
    | NoLine


debug =
    --    True
    False


dataBeacon : Value -> Attribute msg
dataBeacon value =
    attribute "data-beacon" (JE.encode 0 value)


levelContainer level =
    div [ style "margin-left" (String.fromInt (level * 32) ++ "px") ]


itemDisplayTitle : Item -> String
itemDisplayTitle item =
    (if String.trim item.title |> String.isEmpty then
        "<empty>"

     else
        item.title
    )
        ++ (if debug then
                Debug.toString item.id

            else
                ""
           )


classIf bool classValue =
    if bool then
        class classValue

    else
        class ""


viewFlatLineWithConfig : Bool -> FlatLine -> Html Msg
viewFlatLineWithConfig fadeNotDraggable flatLine =
    case flatLine of
        BeaconLine level candidateLocation ->
            levelContainer level
                [ div
                    ([ style "height" "0px"
                     , style "width" "0px"
                     , dataBeacon (candidateLocationEncoder candidateLocation)
                     ]
                        ++ (if debug then
                                [ style "height" "10px"
                                , style "width" "10px"
                                , class "bg-red"
                                ]

                            else
                                []
                           )
                    )
                    [ text " " ]
                ]

        ItemLine level item { isHighlighted, isDraggable } ->
            levelContainer level
                [ div
                    (class "pa1 bb b--black-30 pointer no-selection flex"
                        :: classIf isHighlighted "bg-blue white"
                        :: classIf (not isDraggable && fadeNotDraggable) "o-50"
                        :: (if isDraggable then
                                dragEvents item.id

                            else
                                []
                           )
                    )
                    [ div [ class "flex-auto lh-title", onClick (ItemTitleClicked item.id) ]
                        [ text (itemDisplayTitle item) ]
                    , if isDraggable && isHighlighted then
                        div [ class "ph2", onClick New ] [ text "+" ]

                      else
                        text ""
                    ]
                ]

        EditLine level title ->
            levelContainer level
                [ div
                    (class "pa1 bb b--black-10 pointer no-selection" :: [])
                    [ div [ class "flex lh-title" ]
                        [ input [ class "flex-auto", value title, onInput TitleChanged ] []
                        ]
                    ]
                ]

        NoLine ->
            text ""



--viewFlatLine : FlatLine -> Html Msg
--viewFlatLine =
--    viewFlatLineWithConfig True
--


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
