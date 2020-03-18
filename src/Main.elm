port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import CollapseState exposing (CollapseState(..))
import Dnd exposing (Cursor, XY)
import Html exposing (Attribute, button, div, input, text)
import Html.Attributes as A exposing (attribute, class, disabled, draggable, style, tabindex, value)
import Html.Events as Event exposing (onClick, onInput, preventDefaultOn)
import ItemId exposing (ItemId)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
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
    | Dragging Cursor
    | Browsing


isEditing : Model -> Bool
isEditing { state } =
    case state of
        Editing _ ->
            True

        _ ->
            False


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


type Msg
    = NoOp
    | TitleEditorFocusFailed String
    | OnDragStart ItemId Cursor
    | DM WhenDraggingMsg
    | ItemTitleClicked ItemId
    | TitleChanged String
    | AddNewClicked
    | OnKeyDown KeyEvent
    | OnTab
    | OnShiftTab


cacheDoc : Model -> Cmd msg
cacheDoc =
    .doc >> Doc.encoder >> saveOZ


cacheDocIfChanged : Model -> Model -> Cmd msg
cacheDocIfChanged old new =
    if neqBy .doc old new then
        cacheDoc new

    else
        Cmd.none


effect func ( m, c ) =
    ( m, Cmd.batch [ c, func m ] )


updateWrapper : Msg -> Model -> ( Model, Cmd Msg )
updateWrapper message model =
    update message model
        |> effect (cacheDocIfChanged model)
        |> effect (focusElOnDocCursorChange model)


focusElOnDocCursorChange old new =
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


attemptMapDoc maybeFunc model =
    case maybeFunc model.doc of
        Just doc ->
            { model | doc = doc }

        Nothing ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        TitleEditorFocusFailed domId ->
            Debug.todo ("TitleEditorFocusFailed: " ++ domId)

        OnTab ->
            ( if isEditing model then
                attemptMapDoc Doc.indent model

              else
                model
            , Cmd.none
            )

        OnShiftTab ->
            ( if isEditing model then
                attemptMapDoc Doc.unIndent model

              else
                model
            , Cmd.none
            )

        OnKeyDown ke ->
            ( onKeyDown ke model, Cmd.none )

        AddNewClicked ->
            case model.state of
                Browsing ->
                    ( updateDoc AddNew model, Cmd.none )

                _ ->
                    Debug.todo "impl"

        TitleChanged title ->
            ( case model.state of
                Editing (Edit isAdding _) ->
                    { model | state = Editing (Edit isAdding title) }

                _ ->
                    Debug.todo "Impossible state"
            , Cmd.none
            )

        ItemTitleClicked iid ->
            case model.state of
                Browsing ->
                    let
                        intent =
                            if Doc.currentId model.doc == iid then
                                EditFocused

                            else
                                FocusId iid
                    in
                    ( updateDoc intent model
                    , Cmd.none
                    )

                Editing editState ->
                    ( { model
                        | doc =
                            endEdit editState model.doc
                                |> ignoreNothing (Doc.moveCursorToItemId iid)
                      }
                    , Cmd.none
                    )

                Dragging _ ->
                    Debug.todo "impossible state"

        OnDragStart dragItemId cursor ->
            case model.state of
                Browsing ->
                    case Doc.moveCursorToItemId dragItemId model.doc of
                        Just newDoc ->
                            ( { model | doc = newDoc, state = Dragging cursor }
                            , getBeacons ()
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Editing editState ->
                    case
                        model.doc
                            |> endEdit editState
                            |> Doc.moveCursorToItemId dragItemId
                    of
                        Just newDoc ->
                            ( { model | doc = newDoc, state = Dragging cursor }
                            , getBeacons ()
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Dragging _ ->
                    Debug.todo "impossible state"

        DM msg ->
            case model.state of
                Dragging cursor ->
                    onDndMsg msg cursor model

                _ ->
                    Debug.todo "drag msg received, when not dragging"


onDndMsg : WhenDraggingMsg -> Cursor -> Model -> ( Model, Cmd Msg )
onDndMsg msg cursor model =
    case msg of
        Move clientXY ->
            ( { model | state = Dragging { cursor | clientXY = clientXY } }, getBeacons () )

        Stop ->
            ( { model | state = Browsing }, Cmd.none )

        GotBeacons encodedBeacons ->
            let
                beaconsResult =
                    JD.decodeValue (JD.list Dnd.beaconDecoder) encodedBeacons

                maybeNewDoc =
                    Result.toMaybe beaconsResult
                        |> Maybe.andThen
                            (\beacons ->
                                Dnd.dndClosestCandidateLocation beacons cursor
                                    |> (if debug then
                                            Debug.log "debug"

                                        else
                                            identity
                                       )
                            )
                        |> Maybe.andThen
                            (\cl -> Doc.relocateToCandidateLocation cl model.doc)
            in
            case maybeNewDoc of
                Just newDoc ->
                    ( { model | doc = newDoc }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


onKeyDown : KeyEvent -> Model -> Model
onKeyDown ke model =
    case model.state of
        Browsing ->
            case toUserIntent ke of
                Just intent ->
                    updateDoc intent model

                Nothing ->
                    model

        Editing editState ->
            if KE.hot "Enter" ke then
                { model | doc = endEdit editState model.doc, state = Browsing }

            else if KE.hot "Escape" ke then
                { model | doc = cancelEdit model.doc, state = Browsing }

            else
                model

        Dragging _ ->
            model


type UserIntent
    = EditFocused
    | FocusId ItemId
    | NavPrev
    | CollapseOrNavParent
    | ExpandOrAlternate
    | NavNext
    | UnIndent
    | Indent
    | AddNew
    | MoveUp
    | MoveDown
    | Collapse
    | Expand


toUserIntent : KeyEvent -> Maybe UserIntent
toUserIntent =
    [ ( allPass [ KE.hot " ", KE.targetInputOrButton >> not ], EditFocused )
    , ( allPass [ KE.hot "Enter", KE.targetInputOrButton >> not ], AddNew )
    , ( KE.hot "ArrowUp", NavPrev )
    , ( KE.hot "ArrowDown", NavNext )
    , ( KE.hot "ArrowLeft", CollapseOrNavParent )
    , ( KE.hot "ArrowRight", ExpandOrAlternate )
    , ( KE.ctrl "ArrowUp", MoveUp )
    , ( KE.ctrl "ArrowDown", MoveDown )
    , ( KE.ctrl "ArrowLeft", UnIndent )
    , ( KE.ctrl "ArrowRight", Indent )
    ]
        |> condAlways


updateDoc : UserIntent -> Model -> Model
updateDoc keyboardIntent =
    case keyboardIntent of
        Collapse ->
            attemptMapDoc Doc.collapse

        CollapseOrNavParent ->
            attemptMapDoc Doc.collapseOrNavParent

        ExpandOrAlternate ->
            attemptMapDoc Doc.expandOrGoForward

        Expand ->
            attemptMapDoc Doc.expand

        NavPrev ->
            attemptMapDoc Doc.goBackward

        NavNext ->
            attemptMapDoc Doc.goForward

        UnIndent ->
            attemptMapDoc Doc.unIndent

        Indent ->
            attemptMapDoc Doc.indent

        MoveUp ->
            attemptMapDoc
                Doc.moveBeforePreviousSiblingOrAppendInPreviousSiblingOfParent

        MoveDown ->
            attemptMapDoc
                Doc.moveAfterNextSiblingOrPrependInNextSiblingOfParent

        FocusId id ->
            attemptMapDoc (Doc.moveCursorToItemId id)

        EditFocused ->
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


ignoreNothing : (b -> Maybe b) -> b -> b
ignoreNothing func val =
    func val |> Maybe.withDefault val


focusTitleEditor : Cmd Msg
focusTitleEditor =
    Dom.focus "item-title-editor"
        |> Task.attempt
            (\result ->
                case result of
                    Err (Dom.NotFound domId) ->
                        TitleEditorFocusFailed domId

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
                        TitleEditorFocusFailed domId

                    Ok () ->
                        NoOp
            )


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
                    [ Browser.Events.onMouseMove (JD.map Move clientXYDecoder)
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
        Dragging cursor ->
            let
                xy =
                    Dnd.dndDraggedXY cursor
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


dataBeacon : Value -> Attribute msg
dataBeacon value =
    attribute "data-beacon" (JE.encode 0 value)



-- NODE PARTS VIEW


viewBeacon : CandidateLocation -> HM
viewBeacon candidateLocation =
    div
        ([ style "height" "0px"
         , style "width" "0px"
         , dataBeacon (Doc.candidateLocationEncoder candidateLocation)
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
                , onInput TitleChanged
                , preventDefaultOn "keydown"
                    (KE.decoder
                        |> JD.andThen
                            (\ke ->
                                if KE.hot "Tab" ke then
                                    JD.succeed ( OnTab, True )

                                else if KE.shift "Tab" ke then
                                    JD.succeed ( OnShiftTab, True )

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
                    dragEvents item.id

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


dragEvents : ItemId -> List (Html.Attribute Msg)
dragEvents itemId =
    [ draggable "true"
    , Event.preventDefaultOn "dragstart"
        (JD.map2 (\clientXY offsetXY -> OnDragStart itemId (Cursor clientXY offsetXY))
            clientXYDecoder
            offsetXYDecoder
            |> preventDefault True
        )
    ]


preventDefault : Bool -> Decoder b -> Decoder ( b, Bool )
preventDefault bool =
    JD.map (\msg -> ( msg, bool ))


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
