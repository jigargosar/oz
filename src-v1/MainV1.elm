port module MainV1 exposing (main)

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
import OutlineDoc as Doc
    exposing
        ( CandidateLocation(..)
        , LineInfo
        , OutlineDoc
        )
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
    | EM_OnGlobalKeyDown KeyEvent
    | EM_OnTitleClicked ItemId
    | EM_OnDragStart ItemId Pointer


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
    | HomeClicked
    | ZoomAncestorClicked ItemId
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
            .doc >> Doc.encoder >> saveOZ
    in
    if neqBy .doc old new then
        cacheDoc new

    else
        Cmd.none


focusElOnDocCursorChange : Model -> Model -> Cmd Msg
focusElOnDocCursorChange old new =
    let
        focusHelper =
            Dom.focus
                >> Task.attempt
                    (\result ->
                        case result of
                            Err (Dom.NotFound domId) ->
                                DomFocusFailed domId

                            Ok () ->
                                NoOp
                    )
    in
    if neqBy .state old new || Doc.cursorChanged old.doc new.doc then
        case new.state of
            Editing _ ->
                focusHelper "item-title-editor"

            Browsing ->
                focusHelper "item-title-at-cursor"

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
                    updateWhenEditing (EM_OnGlobalKeyDown ke) editState model

                Dragging _ ->
                    model

        AddNewClicked ->
            case model.state of
                Browsing ->
                    updateWhenBrowsing AddNew model

                _ ->
                    Debug.todo "impl"

        HomeClicked ->
            { model
                | doc =
                    model.doc
                        |> Doc.zoomOutToTop
                        |> Maybe.withDefault model.doc
            }

        ZoomAncestorClicked id ->
            { model
                | doc =
                    model.doc
                        |> Doc.zoomOutToAncestorId id
                        |> Maybe.withDefault model.doc
            }

        ItemTitleClicked itemId ->
            case model.state of
                Browsing ->
                    updateWhenBrowsing (BM_OnTitleClicked itemId) model

                Editing editState ->
                    updateWhenEditing (EM_OnTitleClicked itemId) editState model

                Dragging _ ->
                    Debug.todo "impossible state"

        OnDragStart dragId cursor ->
            case model.state of
                Browsing ->
                    updateWhenBrowsing (BM_OnDragStart dragId cursor) model

                Editing editState ->
                    updateWhenEditing (EM_OnDragStart dragId cursor) editState model

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
        |> Maybe.map (\nd -> setDoc nd model |> setDraggingState pointer)


setDraggingState : Pointer -> Model -> Model
setDraggingState =
    Dragging >> setState


setBrowsingState : Model -> Model
setBrowsingState =
    setState Browsing


getEditingTitle (Edit _ title) =
    title


setEditingTitle title (Edit isAdding _) =
    Edit isAdding title


endEditImplicitAndSwitchToBrowsing : Edit -> Model -> Model
endEditImplicitAndSwitchToBrowsing editState model =
    { model
        | doc =
            model.doc
                |> Doc.setTitleUnlessBlank (getEditingTitle editState)
                |> Doc.removeIfBlankLeaf
        , state = Browsing
    }


updateEditOnEnter : Edit -> Model -> Model
updateEditOnEnter editState model =
    let
        title =
            getEditingTitle editState

        newModel =
            { model
                | doc =
                    model.doc
                        |> Doc.setTitle title
                        |> Doc.removeIfBlankLeaf
                , state = Browsing
            }
    in
    if isBlank title then
        newModel

    else
        updateWhenBrowsing AddNew newModel


cancelEditAndSwitchToBrowsing : Model -> Model
cancelEditAndSwitchToBrowsing model =
    { model | doc = Doc.removeIfBlankLeaf model.doc, state = Browsing }


updateWhenEditing : WhenEditingMsg -> Edit -> Model -> Model
updateWhenEditing msg editState =
    case msg of
        OnTab ->
            mapDocIgnoreNothing Doc.indent

        OnShiftTab ->
            mapDocIgnoreNothing Doc.unIndent

        TitleChanged title ->
            setEditingState (setEditingTitle title editState)

        EM_OnGlobalKeyDown ke ->
            if KE.hot "Enter" ke then
                updateEditOnEnter editState

            else if KE.hot "Escape" ke then
                cancelEditAndSwitchToBrowsing

            else
                identity

        EM_OnTitleClicked itemId ->
            endEditImplicitAndSwitchToBrowsing editState
                >> updateWhenBrowsing (BM_OnTitleClicked itemId)

        EM_OnDragStart itemId pointer ->
            endEditImplicitAndSwitchToBrowsing editState
                >> updateWhenBrowsing (BM_OnDragStart itemId pointer)


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
    = BM_OnTitleClicked ItemId
    | BM_OnGlobalKeyDown KeyEvent
    | BM_OnDragStart ItemId Pointer
    | BM_ExpandAll
    | BM_CollapseAll
    | StartEdit
    | GoBackward
    | CollapseOrGotoParentOrPreviousSibling
    | ExpandOrGotoNext
    | GoForward
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
    [ --( allPass [ KE.hot " ", KE.targetInputOrButton >> not ], StartEdit )
      --, ( allPass [ KE.hot "Enter", KE.targetInputOrButton >> not ], AddNew )
      ( allPass [ KE.hot "Enter", KE.targetInputOrButton >> not ], StartEdit )
    , ( KE.hot "c", BM_CollapseAll )
    , ( KE.hot "x", BM_ExpandAll )
    , ( KE.hot "ArrowUp", GoBackward )
    , ( KE.hot "ArrowDown", GoForward )
    , ( KE.hot "ArrowLeft", CollapseOrGotoParentOrPreviousSibling )
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
        BM_OnDragStart itemId pointer ->
            \model ->
                initDragging itemId pointer model
                    |> Maybe.withDefault model

        BM_OnGlobalKeyDown ke ->
            \model ->
                toBrowsingMsg ke
                    |> Maybe.map (\m -> updateWhenBrowsing m model)
                    |> Maybe.withDefault model

        BM_OnTitleClicked iid ->
            \model ->
                if Doc.currentIdEq iid model.doc then
                    updateWhenBrowsing StartEdit model

                else
                    mapDocIgnoreNothing (Doc.gotoId iid) model

        BM_ExpandAll ->
            mapDocIgnoreNothing Doc.expandAll

        BM_CollapseAll ->
            mapDocIgnoreNothing Doc.collapseAll

        Collapse ->
            mapDocIgnoreNothing Doc.collapse

        ZoomIn ->
            mapDocIgnoreNothing Doc.zoomIn

        ZoomOut ->
            mapDocIgnoreNothing Doc.zoomOut

        CollapseOrGotoParentOrPreviousSibling ->
            mapDocIgnoreNothing
                (firstOf
                    [ Doc.collapse
                    , Doc.gotoParent
                    , Doc.gotoPreviousSibling
                    ]
                )

        ExpandOrGotoNext ->
            mapDocIgnoreNothing (firstOf [ Doc.expand, Doc.goForward ])

        Expand ->
            mapDocIgnoreNothing Doc.expand

        GoBackward ->
            mapDocIgnoreNothing Doc.goBackward

        GoForward ->
            mapDocIgnoreNothing Doc.goForward

        UnIndent ->
            mapDocIgnoreNothing Doc.unIndent

        Indent ->
            mapDocIgnoreNothing Doc.indent

        MoveUp ->
            mapDocIgnoreNothing Doc.moveUpwards

        MoveDown ->
            mapDocIgnoreNothing Doc.moveDownwards

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

        --, htmlMaybe viewZoomAncestors (Doc.zoomInfo doc)
        --, htmlMaybe (\zi -> div [ class "f2 lh-title" ] [ text (itemDisplayTitle zi.current) ])
        --    (Doc.zoomInfo doc)
        , div [] <|
            case state of
                Browsing ->
                    viewBrowsingDoc doc

                Dragging _ ->
                    viewDraggingDoc doc

                Editing (Edit _ title) ->
                    viewEditingDoc title doc
        ]



--viewZoomAncestors : ZoomInfo -> HM
--viewZoomAncestors zi =
--    let
--        container =
--            div [ class "flex-auto flex-grow-0 flex items-center", style "max-width" "100px" ]
--
--        viewSeparator =
--            div [ class "pr1 f5 code gray  " ] [ text ">" ]
--
--        titleStyle =
--            class "pr1 f5 truncate dim pointer"
--
--        viewLink msg title =
--            container [ div [ titleStyle, onClick msg ] [ text title ] ]
--
--        viewHomeLink =
--            viewLink HomeClicked "Home"
--
--        viewAncestorLink ancestor =
--            viewLink (ZoomAncestorClicked ancestor.id) (itemDisplayTitle ancestor)
--
--        viewLastAncestor ancestor =
--            container [ div [ class "pr1 f5 truncate" ] [ text (itemDisplayTitle ancestor) ] ]
--    in
--    div [ class "pv2 flex flex-wrap" ]
--        (viewHomeLink
--            :: List.map viewAncestorLink zi.ancestors
--            ++ [ viewLastAncestor zi.current ]
--            |> List.intersperse viewSeparator
--        )
--


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
                (Doc.viewCurrent
                    (\i ->
                        viewNodeWithoutBeacons (viewItem NotDraggableLine) i
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
    Doc.view
        (\item ->
            viewNodeWithBeacons
                (DraggableLine item.isAtCursor)
                item
        )
        doc


viewDraggingDoc : OutlineDoc -> LHM
viewDraggingDoc doc =
    Doc.view
        (\item ->
            if item.isAtCursorOrDescendentOfCursor then
                viewNodeWithoutBeacons (viewItem FadedLine) item

            else
                viewNodeWithBeacons NotDraggableLine item
        )
        doc


viewEditingDoc : String -> OutlineDoc -> LHM
viewEditingDoc title doc =
    let
        renderItem : LineInfo -> LHM -> HM
        renderItem item =
            if item.isAtCursor then
                wrapWithoutBeacons (viewEditItem title)

            else
                viewNodeWithBeacons (DraggableLine False) item
    in
    Doc.view (\i -> renderItem i) doc



-- NODE VIEW TEMPLATES


viewNodeWithoutBeacons : (a -> HM) -> a -> LHM -> HM
viewNodeWithoutBeacons renderItemFunc item childrenHtml =
    div []
        [ renderItemFunc item
        , div [ class "pl4" ] childrenHtml
        ]


viewNodeWithBeacons : LineVariant -> LineInfo -> LHM -> HM
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


itemDisplayTitle : { a | title : String, id : b } -> String
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


type LineVariant
    = DraggableLine Bool
    | NotDraggableLine
    | FadedLine


viewItem : LineVariant -> LineInfo -> HM
viewItem itemView item =
    let
        { isHighlighted, isDraggable, isFaded } =
            case itemView of
                DraggableLine isHighlighted_ ->
                    { isHighlighted = isHighlighted_, isDraggable = True, isFaded = False }

                NotDraggableLine ->
                    { isHighlighted = False, isDraggable = False, isFaded = False }

                FadedLine ->
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
        [ viewChildStateIndicator item.collapseState
        , div
            [ class "flex-auto lh-title "
            , classIf isHighlighted "bg-lightest-blue black"
            , attrIf isHighlighted tabindex 0
            , attrIf isHighlighted A.id "item-title-at-cursor"
            , onClick (ItemTitleClicked item.id)
            ]
            [ text (itemDisplayTitle item) ]
        , viewAddNewButton isHighlighted |> always noHtml
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



{-

   initialItemGenerator : Generator (List Item)
   initialItemGenerator =
               [ "Quick Brown Fox Jumped Over The Lazy Dog"
               , "Take Notes"
               , "Thou shall not experiment with experiments"
               , "Watch Movies"
               , "Run the mill"
               ]
                   |> List.map OutlineDoc.itemGenerator
                   |> Random.Extra.combine
-}
