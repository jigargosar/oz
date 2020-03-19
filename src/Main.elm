port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import CollapseState exposing (CollapseState(..))
import Dnd exposing (Pointer, XY)
import Html exposing (Attribute, button, div, input, text)
import Html.Attributes as A exposing (class, disabled, style, tabindex, value)
import Html.Events exposing (onClick, onInput, preventDefaultOn)
import ItemId exposing (ItemId)
import Json.Decode as JD exposing (Decoder)
import Json.Encode exposing (Value)
import KeyEvent as KE exposing (KeyEvent)
import OutlineDoc as Doc exposing (CandidateLocation(..), OutlineDoc)
import Random exposing (Generator, Seed)
import Task
import Utils exposing (..)


port getBeacons : () -> Cmd msg


port gotBeacons : (Value -> msg) -> Sub msg


port saveOZ : Value -> Cmd msg



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = updateWrapper
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { doc : OutlineDoc
    , state : State
    , seed : Seed
    }


type State
    = Editing Edit
    | Dragging Pointer
    | Browsing


type Edit
    = Edit Bool String



-- MODEL


type alias Flags =
    { oz : Value, now : Int }


decodeMaybeDoc : Value -> Maybe OutlineDoc
decodeMaybeDoc encodedNullableDoc =
    case JD.decodeValue (JD.nullable Doc.decoder) encodedNullableDoc of
        Ok maybeDoc ->
            maybeDoc

        Err err ->
            Debug.log "oz" (JD.errorToString err)
                |> always Debug.todo "handle doc decode error"


initModel : OutlineDoc -> Seed -> Model
initModel doc seed =
    { doc = doc
    , state = Browsing
    , seed = seed
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initialSeed =
            Random.initialSeed flags.now
    in
    ( case decodeMaybeDoc flags.oz of
        Just doc ->
            initModel doc initialSeed

        Nothing ->
            let
                ( doc, seed ) =
                    Random.step Doc.new initialSeed
            in
            initModel doc seed
    , Cmd.none
    )



-- Update


type WhenDraggingMsg
    = Move XY
    | Stop
    | GotBeacons Value


type WhenEditingMsg
    = TitleChanged String
    | OnTab
    | OnShiftTab


type Msg
    = NoOp
    | DomFocusFailed String
      -- For All States
    | OnKeyDown KeyEvent
      -- Only valid when state is dragging
    | DM WhenDraggingMsg
      -- Only valid when state is editing
    | EM WhenEditingMsg
      -- Valid for both editing & browsing
    | ItemTitleClicked ItemId
    | OnDragStart ItemId Pointer
      -- Only valid when browsing
    | AddNewClicked


updateWrapper : Msg -> Model -> ( Model, Cmd Msg )
updateWrapper message model =
    let
        addEffects newModel =
            ( newModel
            , Cmd.batch
                [ cacheDocIfChanged model newModel
                , focusElOnDocCursorChange model newModel
                , getBeaconsOnDragStartOrDragMove model newModel
                ]
            )
    in
    addEffects (update message model)


cacheDocIfChanged : Model -> Model -> Cmd msg
cacheDocIfChanged old new =
    let
        cacheDoc : Model -> Cmd msg
        cacheDoc =
            reconstructDoc >> Doc.encoder >> saveOZ
    in
    if neqBy .doc old new then
        cacheDoc new

    else
        Cmd.none


reconstructDoc : Model -> OutlineDoc
reconstructDoc model =
    --case model.parent of
    --    Just oldParent ->
    --        Doc.zoomOut model.doc oldParent
    --
    --    Nothing ->
    model.doc


focusElOnDocCursorChange : Model -> Model -> Cmd Msg
focusElOnDocCursorChange old new =
    let
        focusTitleEditor : Cmd Msg
        focusTitleEditor =
            Dom.focus "item-title-editor"
                |> Task.attempt
                    (\result ->
                        case result of
                            Err (Dom.NotFound domId) ->
                                DomFocusFailed domId

                            Ok () ->
                                NoOp
                    )

        focusItemAtCursor : Cmd Msg
        focusItemAtCursor =
            Dom.focus "item-title-at-cursor"
                |> Task.attempt
                    (\result ->
                        case result of
                            Err (Dom.NotFound domId) ->
                                DomFocusFailed domId

                            Ok () ->
                                NoOp
                    )
    in
    if neqBy .state old new || neqBy (.doc >> Doc.ancestorIds) old new then
        case new.state of
            Editing _ ->
                focusTitleEditor

            Browsing ->
                focusItemAtCursor

            Dragging _ ->
                Cmd.none

    else
        Cmd.none


getBeaconsOnDragStartOrDragMove : Model -> Model -> Cmd msg
getBeaconsOnDragStartOrDragMove oldModel newModel =
    let
        isDragging : Model -> Bool
        isDragging model =
            getDragState model /= Nothing

        getDragState : Model -> Maybe Pointer
        getDragState model =
            case model.state of
                Dragging pointer ->
                    Just pointer

                _ ->
                    Nothing
    in
    if isDragging newModel && getDragState oldModel /= getDragState newModel then
        getBeacons ()

    else
        Cmd.none


update : Msg -> Model -> Model
update message model =
    case message of
        NoOp ->
            model

        DomFocusFailed domId ->
            Debug.todo ("DomFocusFailed: " ++ domId)

        OnKeyDown ke ->
            case model.state of
                Browsing ->
                    updateWhenBrowsing (BM_OnGlobalKeyDown ke) model

                Editing editState ->
                    if KE.hot "Enter" ke then
                        { model | doc = endEdit editState model.doc, state = Browsing }

                    else if KE.hot "Escape" ke then
                        { model | doc = cancelEdit model.doc, state = Browsing }

                    else
                        model

                Dragging _ ->
                    model

        AddNewClicked ->
            case model.state of
                Browsing ->
                    updateWhenBrowsing AddNew model

                _ ->
                    Debug.todo "impl"

        ItemTitleClicked itemId ->
            case model.state of
                Browsing ->
                    updateWhenBrowsing (BM_TitleClicked itemId) model

                Editing editState ->
                    model
                        |> mapDoc (endEdit editState)
                        |> gotoIdIgnoreNothing itemId

                Dragging _ ->
                    Debug.todo "impossible state"

        OnDragStart dragId cursor ->
            case model.state of
                Browsing ->
                    initDraggingIgnoreNothing dragId cursor model

                Editing editState ->
                    model
                        |> mapDoc (endEdit editState)
                        |> initDraggingIgnoreNothing dragId cursor

                Dragging _ ->
                    Debug.todo "impossible state"

        DM msg ->
            case model.state of
                Dragging cursor ->
                    updateWhenDragging msg cursor model

                _ ->
                    Debug.todo "drag msg received, when not dragging"

        EM msg ->
            case model.state of
                Editing editState ->
                    updateWhenEditing msg editState model

                _ ->
                    Debug.todo "drag msg received, when not dragging"


setDoc : OutlineDoc -> Model -> Model
setDoc doc model =
    { model | doc = doc }


mapDoc : (OutlineDoc -> OutlineDoc) -> Model -> Model
mapDoc func model =
    setDoc (func model.doc) model


mapDocIgnoreNothing : (OutlineDoc -> Maybe OutlineDoc) -> Model -> Model
mapDocIgnoreNothing maybeFunc model =
    case maybeFunc model.doc of
        Just doc ->
            setDoc doc model

        Nothing ->
            model


setState : State -> Model -> Model
setState state model =
    { model | state = state }


setEditingState : Edit -> Model -> Model
setEditingState =
    Editing >> setState


initDragging : ItemId -> Pointer -> Model -> Maybe Model
initDragging dragId pointer model =
    Doc.gotoId dragId model.doc
        |> Maybe.map
            (\nd ->
                setDoc nd model
                    |> setDraggingState pointer
            )


initDraggingIgnoreNothing : ItemId -> Pointer -> Model -> Model
initDraggingIgnoreNothing itemId pointer model =
    initDragging itemId pointer model |> Maybe.withDefault model


setDraggingState : Pointer -> Model -> Model
setDraggingState =
    Dragging >> setState


setBrowsingState : Model -> Model
setBrowsingState =
    setState Browsing


gotoIdIgnoreNothing : ItemId -> Model -> Model
gotoIdIgnoreNothing itemId =
    mapDocIgnoreNothing (Doc.gotoId itemId)


zoomInIgnoreNothing : Model -> Model
zoomInIgnoreNothing =
    mapDocIgnoreNothing Doc.zoomIn


zoomOutIgnoreNothing : Model -> Model
zoomOutIgnoreNothing =
    mapDocIgnoreNothing Doc.zoomOut


updateWhenEditing : WhenEditingMsg -> Edit -> Model -> Model
updateWhenEditing msg (Edit isAdding _) =
    case msg of
        OnTab ->
            mapDocIgnoreNothing Doc.indent

        OnShiftTab ->
            mapDocIgnoreNothing Doc.unIndent

        TitleChanged title ->
            setEditingState (Edit isAdding title)


updateWhenDragging : WhenDraggingMsg -> Pointer -> Model -> Model
updateWhenDragging msg pointer model =
    case msg of
        Move clientXY ->
            setDraggingState (Dnd.setClientXY clientXY pointer) model

        Stop ->
            setBrowsingState model

        GotBeacons encodedBeacons ->
            case Dnd.closestCandidateResult pointer encodedBeacons of
                Ok cl ->
                    mapDocIgnoreNothing (Doc.relocateTo cl) model

                Err err ->
                    Debug.todo ("GotBeacons Error: " ++ err)


type BrowsingMsg
    = BM_TitleClicked ItemId
    | BM_OnGlobalKeyDown KeyEvent
    | StartEdit
      --| BM_DocMsg DocMsg
    | GotoPrev
    | CollapseOrGotoParent
    | ExpandOrGotoNext
    | GotoNext
    | UnIndent
    | Indent
    | ZoomIn
    | ZoomOut
    | AddNew
    | MoveUp
    | MoveDown
    | Collapse
    | Expand


toBrowsingMsg : KeyEvent -> Maybe BrowsingMsg
toBrowsingMsg =
    [ ( allPass [ KE.hot " ", KE.targetInputOrButton >> not ], StartEdit )
    , ( allPass [ KE.hot "Enter", KE.targetInputOrButton >> not ], AddNew )
    , ( KE.hot "ArrowUp", GotoPrev )
    , ( KE.hot "ArrowDown", GotoNext )
    , ( KE.hot "ArrowLeft", CollapseOrGotoParent )
    , ( KE.hot "ArrowRight", ExpandOrGotoNext )
    , ( KE.ctrl "ArrowUp", MoveUp )
    , ( KE.ctrl "ArrowDown", MoveDown )
    , ( KE.ctrl "ArrowLeft", UnIndent )
    , ( KE.ctrl "ArrowRight", Indent )
    , ( KE.shiftAlt "ArrowLeft", ZoomOut )
    , ( KE.shiftAlt "ArrowRight", ZoomIn )
    ]
        |> condAlways


updateWhenBrowsing : BrowsingMsg -> Model -> Model
updateWhenBrowsing message =
    case message of
        --BM_DocMsg msg ->
        --    updateDoc msg
        BM_OnGlobalKeyDown ke ->
            \model ->
                toBrowsingMsg ke
                    |> Maybe.map (Debug.log "bm" >> (\m -> updateWhenBrowsing m model))
                    |> Maybe.withDefault model

        BM_TitleClicked iid ->
            \model ->
                if Doc.currentId model.doc == iid then
                    updateWhenBrowsing StartEdit model

                else
                    gotoIdIgnoreNothing iid model

        Collapse ->
            mapDocIgnoreNothing Doc.collapse

        ZoomIn ->
            zoomInIgnoreNothing

        ZoomOut ->
            zoomOutIgnoreNothing

        CollapseOrGotoParent ->
            mapDocIgnoreNothing Doc.collapseOrNavParent

        ExpandOrGotoNext ->
            mapDocIgnoreNothing Doc.expandOrGoForward

        Expand ->
            mapDocIgnoreNothing Doc.expand

        GotoPrev ->
            mapDocIgnoreNothing Doc.goBackward

        GotoNext ->
            mapDocIgnoreNothing Doc.goForward

        UnIndent ->
            mapDocIgnoreNothing Doc.unIndent

        Indent ->
            mapDocIgnoreNothing Doc.indent

        MoveUp ->
            mapDocIgnoreNothing
                Doc.moveBeforePreviousSiblingOrAppendInPreviousSiblingOfParent

        MoveDown ->
            mapDocIgnoreNothing
                Doc.moveAfterNextSiblingOrPrependInNextSiblingOfParent

        StartEdit ->
            initEditState

        AddNew ->
            addNewLine


initEditState : Model -> Model
initEditState model =
    { model | state = Editing (Edit False (Doc.currentTitle model.doc)) }


addNewLine : Model -> Model
addNewLine model =
    let
        ( newDoc, newModel ) =
            generate (Doc.addNew model.doc) model
    in
    { newModel | doc = newDoc }
        |> initEditNewState


initEditNewState : Model -> Model
initEditNewState model =
    { model | state = Editing (Edit True (Doc.currentTitle model.doc)) }


endEdit : Edit -> OutlineDoc -> OutlineDoc
endEdit (Edit _ title) doc =
    doc
        |> Doc.setTitleUnlessBlank title
        |> Doc.removeIfBlankLeaf


cancelEdit : OutlineDoc -> OutlineDoc
cancelEdit doc =
    doc
        |> Doc.removeIfBlankLeaf


generate : Generator a -> Model -> ( a, Model )
generate generator model =
    let
        ( a, seed ) =
            Random.step generator model.seed
    in
    ( a, { model | seed = seed } )


subscriptions : Model -> Sub Msg
subscriptions m =
    Sub.batch
        [ case m.state of
            Browsing ->
                Sub.none

            Editing _ ->
                Sub.none

            Dragging _ ->
                Sub.batch
                    [ Browser.Events.onMouseMove (JD.map Move Dnd.clientXYDecoder)
                    , Browser.Events.onMouseUp (JD.succeed Stop)
                    , gotBeacons GotBeacons
                    ]
                    |> Sub.map DM
        , Browser.Events.onKeyDown (KE.decoder |> JD.map OnKeyDown)
        ]



-- View


view : Model -> HM
view m =
    div [ class "pv3 ph5 measure-narrow f3 lh-copy" ]
        [ viewOutline m.state m.doc
        , viewDraggedNode m.state m.doc
        ]



--  Outline View


type alias HM =
    Html.Html Msg


type alias LHM =
    List (Html.Html Msg)


viewOutline : State -> OutlineDoc -> HM
viewOutline state doc =
    div []
        [ div [ class "f1" ] [ text "OZ Outlining" ]
        , div [] <|
            case state of
                Browsing ->
                    viewBrowsingDoc doc

                Dragging _ ->
                    viewDraggingDoc doc

                Editing (Edit _ title) ->
                    viewEditingDoc title doc
        ]


viewDraggedNode : State -> OutlineDoc -> HM
viewDraggedNode state doc =
    case state of
        Dragging pointer ->
            let
                xy =
                    Dnd.pointerXY pointer
            in
            div
                [ class "fixed no-pe"
                , style "left" (String.fromFloat xy.x ++ "px")
                , style "top" (String.fromFloat xy.y ++ "px")
                ]
                (Doc.restructureCurrentNode
                    (\( i, _ ) ->
                        viewNodeWithoutBeacons (viewItem NotDraggableItem) i
                    )
                    doc
                )

        Editing _ ->
            text ""

        Browsing ->
            text ""



-- OUTLINE VIEWS


viewBrowsingDoc : OutlineDoc -> LHM
viewBrowsingDoc doc =
    let
        highlightedId =
            Doc.currentId doc
    in
    Doc.restructureWithContext
        (\( item, _ ) ->
            viewNodeWithBeacons
                (DraggableItem (item.id == highlightedId))
                item
        )
        doc


type alias LineItem =
    { id : ItemId
    , title : String
    , collapsed : CollapseState
    }


viewDraggingDoc : OutlineDoc -> LHM
viewDraggingDoc doc =
    let
        draggedId =
            Doc.currentId doc
    in
    Doc.restructureWithContext
        (\( item, ancestorsIds ) ->
            if List.any ((==) draggedId) (item.id :: ancestorsIds) then
                viewNodeWithoutBeacons (viewItem FadedItem) item

            else
                viewNodeWithBeacons NotDraggableItem item
        )
        doc


viewEditingDoc : String -> OutlineDoc -> LHM
viewEditingDoc title doc =
    let
        editItemId =
            Doc.currentId doc

        renderItem : LineItem -> LHM -> HM
        renderItem item =
            if item.id == editItemId then
                wrapWithoutBeacons (viewEditItem title)

            else
                viewNodeWithBeacons (DraggableItem False) item
    in
    Doc.restructureWithContext (\( i, _ ) -> renderItem i) doc



-- NODE VIEW TEMPLATES


viewNodeWithoutBeacons : (a -> HM) -> a -> LHM -> HM
viewNodeWithoutBeacons renderItemFunc item childrenHtml =
    div []
        [ renderItemFunc item
        , div [ class "pl4" ] childrenHtml
        ]


viewNodeWithBeacons : ItemVariant -> LineItem -> LHM -> HM
viewNodeWithBeacons itemView item =
    wrapWithBeacons (viewItem itemView item) item.id


wrapWithoutBeacons : HM -> LHM -> HM
wrapWithoutBeacons nodeHtml childrenHtml =
    div []
        [ nodeHtml
        , div [ class "pl4" ] childrenHtml
        ]


wrapWithBeacons : HM -> ItemId -> LHM -> HM
wrapWithBeacons itemHtml itemId childrenHtml =
    div []
        [ viewBeacon (Doc.before itemId)
        , itemHtml
        , div [ class "pl4" ]
            (viewBeacon (Doc.prependIn itemId)
                :: childrenHtml
                ++ [ viewBeacon (Doc.appendIn itemId) ]
            )
        , viewBeacon (Doc.after itemId)
        ]



-- NODE PARTS VIEW


viewBeacon : CandidateLocation -> HM
viewBeacon candidateLocation =
    div
        ([ style "height" "0px"
         , style "width" "0px"
         , Dnd.beaconAttr candidateLocation
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


viewEditItem : String -> HM
viewEditItem title =
    div
        [ class "pa1 bb b--black-10 pointer no-selection" ]
        [ div [ class "flex lh-title" ]
            [ input
                [ A.id "item-title-editor"
                , class "flex-auto"
                , value title
                , onInput (EM << TitleChanged)
                , preventDefaultOn "keydown"
                    (KE.decoder
                        |> JD.andThen
                            (\ke ->
                                if KE.hot "Tab" ke then
                                    JD.succeed ( EM OnTab, True )

                                else if KE.shift "Tab" ke then
                                    JD.succeed ( EM OnShiftTab, True )

                                else
                                    JD.fail ""
                            )
                    )
                ]
                []
            ]
        ]


debug =
    --    True
    False


itemDisplayTitle : LineItem -> String
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


attrIf bool attrFunc attrValue =
    if bool then
        attrFunc attrValue

    else
        class ""


type ItemVariant
    = DraggableItem Bool
    | NotDraggableItem
    | FadedItem


viewItem : ItemVariant -> LineItem -> HM
viewItem itemView item =
    let
        { isHighlighted, isDraggable, isFaded } =
            case itemView of
                DraggableItem isHighlighted_ ->
                    { isHighlighted = isHighlighted_, isDraggable = True, isFaded = False }

                NotDraggableItem ->
                    { isHighlighted = False, isDraggable = False, isFaded = False }

                FadedItem ->
                    { isHighlighted = False, isDraggable = False, isFaded = True }
    in
    div
        (class "pa1 bb b--black-30 pointer no-selection flex"
            :: classIf isFaded "o-50"
            :: (if isDraggable then
                    Dnd.dragEvents (OnDragStart item.id)

                else
                    []
               )
        )
        [ viewChildStateIndicator item.collapsed
        , div
            [ class "flex-auto lh-title "
            , classIf isHighlighted "bg-blue white"
            , attrIf isHighlighted tabindex 0
            , attrIf isHighlighted A.id "item-title-at-cursor"
            , onClick (ItemTitleClicked item.id)
            ]
            [ text (itemDisplayTitle item) ]
        , viewAddNewButton isHighlighted
        ]


viewChildStateIndicator : CollapseState -> HM
viewChildStateIndicator collapseState =
    div [ class "mr2 self-start dim pointer code" ]
        (case collapseState of
            NoChildren ->
                [ text "." ]

            Collapsed ->
                [ text "+" ]

            Expanded ->
                [ text "-" ]
        )


viewAddNewButton : Bool -> HM
viewAddNewButton visible =
    button
        ([ class "ph2 pv0 self-start lh-title bn bg-inherit color-inherit"
         , onClick AddNewClicked
         ]
            ++ (if visible then
                    []

                else
                    [ class "pe-none o-0", disabled True ]
               )
        )
        [ text "+" ]
