port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Forest.Tree exposing (Forest)
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes as A exposing (attribute, class, draggable, style, value)
import Html.Events as Event exposing (onClick, onInput)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import OutlineDoc exposing (CandidateLocation(..), Item, ItemId, OutlineDoc, OutlineNode)
import Random exposing (Generator, Seed)
import Task


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
    { outline : Outline
    , seed : Seed
    }


type Outline
    = NoDoc
    | Browsing OutlineDoc
    | Dragging Cursor OutlineDoc
    | Editing OutlineDoc String



-- OUTLINE DRAG AND DROP


type alias Cursor =
    { clientXY : XY
    , offsetXY : XY
    }



-- DRAG AND DROP BEACON: HELPS DETERMINE DROP INTENT


type alias Beacon =
    ( CandidateLocation, Rect )



-- MODEL


type alias Flags =
    { oz : Value, now : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        oz =
            case JD.decodeValue (JD.nullable OutlineDoc.decoder) flags.oz of
                Ok got ->
                    got

                Err err ->
                    Debug.log "oz" (JD.errorToString err)
                        |> always Nothing
    in
    ( { outline = Maybe.map Browsing oz |> Maybe.withDefault NoDoc
      , seed = Random.initialSeed flags.now
      }
    , Cmd.none
    )



-- DND HELPERS


dndDraggedXY : Cursor -> XY
dndDraggedXY dnd =
    subtractXY dnd.clientXY dnd.offsetXY


dndClosestCandidateLocation : List Beacon -> Cursor -> Maybe CandidateLocation
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
        (JD.field "id" OutlineDoc.candidateLocationDecoder)
        rectDecoder



-- Update


type Msg
    = NoOp
    | TitleEditorFocusFailed String
    | OnDragStart ItemId Cursor
    | Move XY
    | Stop
    | GotBeacons Value
    | ItemTitleClicked ItemId
    | TitleChanged String
    | New
    | OnKeyDown KeyEvent


cacheDocIfChanged : OutlineDoc -> OutlineDoc -> Cmd msg
cacheDocIfChanged oldOZ newOZ =
    if oldOZ /= newOZ then
        newOZ |> (OutlineDoc.encoder >> saveOZ)

    else
        Cmd.none


outlineToDoc : Outline -> Maybe OutlineDoc
outlineToDoc outline =
    case outline of
        NoDoc ->
            Nothing

        Browsing oz ->
            Just oz

        Dragging _ oz ->
            Just oz

        Editing oz _ ->
            Just oz


cacheOutlineOnChangeCmd : Outline -> Outline -> Cmd msg
cacheOutlineOnChangeCmd oldOutline newOutline =
    Maybe.map2 cacheDocIfChanged (outlineToDoc oldOutline) (outlineToDoc newOutline)
        |> Maybe.withDefault Cmd.none


updateWrapper : Msg -> Model -> ( Model, Cmd Msg )
updateWrapper =
    let
        focusEditorOnStartEdit oldModel ( newModel, cmd ) =
            case ( oldModel.outline, newModel.outline ) of
                ( Editing _ _, _ ) ->
                    ( newModel, cmd )

                ( _, Editing _ _ ) ->
                    ( newModel, Cmd.batch [ cmd, focusTitleEditor ] )

                _ ->
                    ( newModel, cmd )

        persistModelOnChange oldModel ( newModel, cmd ) =
            ( newModel, Cmd.batch [ cmd, cacheOutlineOnChangeCmd oldModel.outline newModel.outline ] )

        helper message model =
            update message model
                |> focusEditorOnStartEdit model
                |> persistModelOnChange model
    in
    helper


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        TitleEditorFocusFailed domId ->
            Debug.todo ("TitleEditorFocusFailed: " ++ domId)

        OnKeyDown ke ->
            case model.outline of
                Browsing doc ->
                    case ke.key of
                        "Enter" ->
                            ( { model | outline = initEdit doc }, Cmd.none )

                        "o" ->
                            ( let
                                ( newDoc, newModel ) =
                                    generate (OutlineDoc.addNewLine "" doc) model
                              in
                              { newModel | outline = initEdit newDoc }
                            , Cmd.none
                            )

                        "ArrowUp" ->
                            ( { model
                                | outline =
                                    Browsing (ignoreNothing OutlineDoc.goBackward doc)
                              }
                            , Cmd.none
                            )

                        "ArrowDown" ->
                            ( { model
                                | outline =
                                    Browsing (ignoreNothing OutlineDoc.goForward doc)
                              }
                            , Cmd.none
                            )

                        "ArrowLeft" ->
                            ( { model
                                | outline =
                                    Browsing (ignoreNothing OutlineDoc.moveAfterParent doc)
                              }
                            , Cmd.none
                            )

                        "ArrowRight" ->
                            ( { model
                                | outline =
                                    Browsing (ignoreNothing OutlineDoc.appendInPreviousSibling doc)
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                NoDoc ->
                    ( model, Cmd.none )

                Dragging _ _ ->
                    ( model, Cmd.none )

                Editing doc title ->
                    case ke.key of
                        "Enter" ->
                            ( { model | outline = endEditAndInitBrowsing title doc }
                            , Cmd.none
                            )

                        "Escape" ->
                            ( { model | outline = cancelEditAndInitBrowsing doc }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

        New ->
            case model.outline of
                Browsing doc ->
                    ( let
                        ( newDoc, newModel ) =
                            generate (OutlineDoc.addNewLine "" doc) model
                      in
                      { newModel | outline = initEdit newDoc }
                    , Cmd.none
                    )

                _ ->
                    Debug.todo "impl"

        TitleChanged title ->
            ( { model | outline = onEditTitleChanged title model.outline }, Cmd.none )

        ItemTitleClicked iid ->
            case model.outline of
                NoDoc ->
                    Debug.todo "impossible state"

                Browsing doc ->
                    if currentId doc == iid then
                        ( { model | outline = initEdit doc }, Cmd.none )

                    else
                        case OutlineDoc.focusId iid doc of
                            Just focusedDoc ->
                                ( { model | outline = Browsing focusedDoc }, Cmd.none )

                            Nothing ->
                                ( model, Cmd.none )

                Dragging _ _ ->
                    Debug.todo "impossible state"

                Editing doc title ->
                    ( { model | outline = endEditAndBrowseId iid title doc }, Cmd.none )

        OnDragStart dragItemId cursor ->
            case model.outline of
                Browsing oz ->
                    case OutlineDoc.focusId dragItemId oz of
                        Just noz ->
                            ( { model | outline = Dragging cursor noz }
                            , getBeacons ()
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Editing doc title ->
                    case
                        endEditAndStartDraggingId dragItemId cursor title doc
                    of
                        Just outline ->
                            ( { model | outline = outline }
                            , getBeacons ()
                            )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    Debug.todo "impl"

        Move clientXY ->
            case model.outline of
                Dragging dnd oz ->
                    ( { model | outline = Dragging { dnd | clientXY = clientXY } oz }, getBeacons () )

                _ ->
                    Debug.todo "impossible state"

        Stop ->
            case model.outline of
                Dragging _ oz ->
                    ( { model | outline = Browsing oz }, Cmd.none )

                _ ->
                    Debug.todo "impossible state"

        GotBeacons encodedBeacons ->
            case model.outline of
                Dragging dnd oz ->
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
                                    (\cl -> OutlineDoc.moveToCandidateLocation cl oz)
                    in
                    case maybeNoz of
                        Just noz ->
                            ( { model | outline = Dragging dnd noz }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    Debug.todo "impossible state"


endEdit : String -> OutlineDoc -> OutlineDoc
endEdit title doc =
    doc
        |> OutlineDoc.setTitleUnlessBlank title
        |> OutlineDoc.removeIfBlankLeaf


cancelEdit : OutlineDoc -> OutlineDoc
cancelEdit doc =
    doc
        |> OutlineDoc.removeIfBlankLeaf


endEditAndInitBrowsing : String -> OutlineDoc -> Outline
endEditAndInitBrowsing title =
    endEdit title >> Browsing


endEditAndBrowseId : ItemId -> String -> OutlineDoc -> Outline
endEditAndBrowseId id title =
    endEdit title >> ignoreNothing (OutlineDoc.focusId id) >> Browsing


endEditAndStartDraggingId : ItemId -> Cursor -> String -> OutlineDoc -> Maybe Outline
endEditAndStartDraggingId dragId cursor title =
    endEdit title >> OutlineDoc.focusId dragId >> Maybe.map (Dragging cursor)


cancelEditAndInitBrowsing : OutlineDoc -> Outline
cancelEditAndInitBrowsing =
    cancelEdit >> Browsing


initEdit : OutlineDoc -> Outline
initEdit doc =
    Editing doc (OutlineDoc.currentTitle doc)


onEditTitleChanged : String -> Outline -> Outline
onEditTitleChanged title outline =
    case outline of
        Editing doc _ ->
            Editing doc title

        _ ->
            Debug.todo "Impossible state"


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


currentId : OutlineDoc -> ItemId
currentId =
    OutlineDoc.currentId


subscriptions : Model -> Sub Msg
subscriptions m =
    Sub.batch
        [ case m.outline of
            NoDoc ->
                Sub.none

            Browsing _ ->
                Sub.none

            Dragging _ _ ->
                Sub.batch
                    [ Browser.Events.onMouseMove (JD.map Move clientXYDecoder)
                    , Browser.Events.onMouseUp (JD.succeed Stop)
                    , gotBeacons GotBeacons
                    ]

            Editing _ _ ->
                Sub.none
        , Browser.Events.onKeyDown (JD.map OnKeyDown keyEventDecoder)
        ]



-- KEY EVENT


type alias KeyEvent =
    { key : String
    , ctrl : Bool
    }


keyEventDecoder : Decoder KeyEvent
keyEventDecoder =
    JD.succeed KeyEvent
        |> JD.map2 (|>) (JD.field "key" JD.string)
        |> JD.map2 (|>) (JD.field "ctrlKey" JD.bool)



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
        NoDoc ->
            []

        Browsing doc ->
            let
                highlightedId =
                    currentId doc
            in
            OutlineDoc.restructure
                (\item -> renderDraggableWithBeacons (item.id == highlightedId) item)
                doc

        Dragging _ doc ->
            let
                draggedId =
                    OutlineDoc.currentId doc

                renderForestFns : List (Bool -> HM)
                renderForestFns =
                    OutlineDoc.restructure
                        (\item renderChildrenFns ->
                            \shouldRenderWithoutBeacon ->
                                let
                                    children bool =
                                        List.map (\f -> f bool) renderChildrenFns
                                in
                                if shouldRenderWithoutBeacon || item.id == draggedId then
                                    renderWithoutBeacons item (children True)

                                else
                                    renderNotDraggableWithBeacons item (children False)
                        )
                        doc
            in
            List.map (\fn -> fn False) renderForestFns

        Editing doc title ->
            let
                editItemId =
                    currentId doc

                renderItem : Item -> LHM -> HM
                renderItem item =
                    if item.id == editItemId then
                        renderEditItem title

                    else
                        renderDraggableWithBeacons False item
            in
            OutlineDoc.restructure renderItem doc


viewDraggedNode : Outline -> Html Msg
viewDraggedNode outline =
    case outline of
        NoDoc ->
            text ""

        Browsing _ ->
            text ""

        Dragging dnd doc ->
            let
                xy =
                    dndDraggedXY dnd
            in
            div
                [ class "fixed no-pe"
                , style "left" (String.fromFloat xy.x ++ "px")
                , style "top" (String.fromFloat xy.y ++ "px")
                ]
                [ OutlineDoc.restructureFocused renderDraggedItem doc ]

        Editing _ _ ->
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
                     , dataBeacon (OutlineDoc.candidateLocationEncoder candidateLocation)
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
                        [ input [ A.id "item-title-editor", class "flex-auto", value title, onInput TitleChanged ] []
                        ]
                    ]
                ]

        NoLine ->
            text ""


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
