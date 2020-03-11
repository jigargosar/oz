module Main exposing (main)

-- Browser.Element Scaffold

import Browser.Dom as Dom exposing (Element)
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Attribute, Html, div, text)
import Html.Attributes exposing (class, draggable)
import Html.Events
import Json.Decode as JD exposing (Decoder)
import List.Extra
import Task
import TimeTravel.Browser



-- Model


type alias Item =
    { id : String
    , title : String
    }


type alias DI =
    { dragId : String
    , dragEl : Element
    , dropId : String
    , dropEl : Element
    , ce : DomEv
    , elDict : Dict String Element
    }


type alias DomEv =
    { x : Int
    , y : Int
    , pageX : Int
    , pageY : Int
    }


domeEvDecoder : Decoder DomEv
domeEvDecoder =
    let
        intF name =
            JD.field name JD.int
    in
    JD.map4 DomEv
        (intF "x")
        (intF "y")
        (intF "pageX")
        (intF "pageY")


type alias Model =
    { items : List Item
    , di : Maybe DI
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { items =
            [ "A very very long item title which should wrap"
            , "Small title"
            , "Another long warping title, for experiment"
            , "Next small title."
            , "Small title"
            ]
                |> List.indexedMap (\i t -> Item (String.fromInt i) t)
      , di = Nothing
      }
    , Cmd.none
    )



-- Update


type alias ElResult =
    Result Dom.Error Element


type Msg
    = NoOp
    | OnDragMove DomEv
    | OnDragStart { itemId : String, domId : String } DomEv
    | OnDragStartWithEl String DomEv ElResult
    | OnDragOver { itemId : String, domId : String }
    | OnDragOverWithEl String ElResult
    | OnDragEnd
    | GotItemEl String ElResult


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        OnDragStart { domId, itemId } ev ->
            ( model, Dom.getElement domId |> Task.attempt (OnDragStartWithEl itemId ev) )

        OnDragStartWithEl _ _ (Err _) ->
            ( model, Cmd.none )

        OnDragStartWithEl itemId ev (Ok dragEl) ->
            let
                di : DI
                di =
                    { dragId = itemId
                    , dragEl = dragEl
                    , dropId = itemId
                    , dropEl = dragEl
                    , ce = ev
                    , elDict = Dict.empty
                    }
            in
            ( { model | di = Just di }, refreshAllItemElCmd model.items )

        GotItemEl _ (Err _) ->
            ( model, Cmd.none )

        GotItemEl id (Ok el) ->
            ( mapMaybeDI (\di -> { di | elDict = Dict.insert id el di.elDict }) model
            , Cmd.none
            )

        OnDragOver { itemId, domId } ->
            ( model, Dom.getElement domId |> Task.attempt (OnDragOverWithEl itemId) )

        OnDragOverWithEl _ (Err _) ->
            ( model, Cmd.none )

        OnDragOverWithEl dropId (Ok dropEl) ->
            case model.di of
                Just di ->
                    ( { model | di = Just { di | dropId = dropId, dropEl = dropEl } }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        OnDragMove domEv ->
            ( mapMaybeDI (\di -> { di | ce = domEv }) model
            , refreshAllItemElCmd model.items
            )

        OnDragEnd ->
            ( { model | di = Nothing }, Cmd.none )


refreshAllItemElCmd : List Item -> Cmd Msg
refreshAllItemElCmd items =
    let
        getItemElement : Item -> Cmd Msg
        getItemElement item =
            itemToDomId item
                |> Dom.getElement
                |> Task.attempt (GotItemEl item.id)
    in
    Cmd.batch (List.map getItemElement items)


itemToDomId : Item -> String
itemToDomId item =
    "item-el-id-" ++ item.id


mapMaybeDI func m =
    { m | di = Maybe.map func m.di }


subscriptions : Model -> Sub Msg
subscriptions m =
    Sub.batch
        [ case m.di of
            Just _ ->
                Sub.batch
                    [ Browser.Events.onMouseUp (JD.succeed OnDragEnd)
                    , Browser.Events.onMouseMove (JD.map OnDragMove domeEvDecoder)
                    ]

            Nothing ->
                Sub.none
        ]



--updateDnDItems : Model -> Model
--updateDnDItems model =
--    let
--        setDiAndItemsTuple ( di, items ) =
--            { model | di = Just di, items = items }
--    in
--    case model.di of
--        Just di ->
--            sortDndItems di model.items
--                |> setDiAndItemsTuple
--
--        Nothing ->
--            model
--
--
--sortDndItems : DI -> List Item -> ( DI, List Item )
--sortDndItems di items =
--    let
--        y : Float
--        y =
--            toFloat di.ce.pageY
--
--        dropElY : Float
--        dropElY =
--            di.dropEl.element.y
--
--        dragElHeight : Float
--        dragElHeight =
--            di.dragEl.element.height
--
--        shouldSort =
--            if di.dragIdx_ < di.dropIdx then
--                -- Note: We didn't have to fix this since, once the flip happens
--                -- flop is automatically stopped by next condition
--                True
--
--            else if di.dragIdx_ > di.dropIdx then
--                y < (dropElY + dragElHeight)
--
--            else
--                False
--    in
--    if shouldSort then
--        ( { di | dragIdx_ = di.dropIdx, dropEl = di.dragEl }
--        , removeAtThenInsertAt di.dragIdx_ di.dropIdx items
--        )
--
--    else
--        ( di, items )
--
--


removeAtThenInsertAt : Int -> Int -> List a -> List a
removeAtThenInsertAt removeIdx insertIdx items =
    case List.Extra.getAt removeIdx items of
        Nothing ->
            items

        Just item ->
            case
                List.Extra.removeAt removeIdx items
                    |> List.Extra.splitAt insertIdx
            of
                ( left, right ) ->
                    left ++ item :: right



-- View


peq func val obj =
    func obj == val


dndSortItems : DI -> List Item -> List Item
dndSortItems di items =
    let
        closestIdElPair : Maybe ( String, Element )
        closestIdElPair =
            di.elDict
                |> Dict.map
                    (\_ { element } ->
                        abs (element.y - toFloat di.ce.pageY)
                    )
                |> Dict.toList
                |> List.Extra.minimumBy Tuple.second
                |> Maybe.map Tuple.first
                |> Maybe.andThen (\id -> Dict.get id di.elDict |> Maybe.map (Tuple.pair id))

        shouldPlaceBeforeEl el =
            toFloat di.ce.pageY < el.y + (el.height / 2)

        res2 : ( String, Element ) -> List Item
        res2 ( dropId, el ) =
            if dropId == di.dragId then
                items

            else if shouldPlaceBeforeEl el.element then
                moveBeforeBy (peq .id di.dragId) (peq .id dropId) items

            else
                moveAfterBy (peq .id di.dragId) (peq .id dropId) items
    in
    case closestIdElPair of
        Just ip ->
            res2 ip

        Nothing ->
            Debug.todo "closest id pair not found"


getAndRemoveBy : (a -> Bool) -> List a -> Maybe ( a, List a )
getAndRemoveBy pred list =
    Maybe.map2 (\idx item -> ( item, List.Extra.removeAt idx list ))
        (List.Extra.findIndex pred list)
        (List.Extra.find pred list)


moveBeforeBy : (a -> Bool) -> (a -> Bool) -> List a -> List a
moveBeforeBy toMoveItemPred beforeItemPred list =
    case getAndRemoveBy toMoveItemPred list of
        Nothing ->
            list

        Just ( toMove, withoutToMove ) ->
            case
                List.Extra.findIndex beforeItemPred withoutToMove
                    |> Maybe.map (\i -> List.Extra.splitAt i withoutToMove)
            of
                Nothing ->
                    list

                Just ( left, right ) ->
                    left ++ toMove :: right


moveAfterBy : (a -> Bool) -> (a -> Bool) -> List a -> List a
moveAfterBy toMoveItemPred beforeItemPred list =
    case getAndRemoveBy toMoveItemPred list of
        Nothing ->
            list

        Just ( toMove, withoutToMove ) ->
            case
                List.Extra.findIndex beforeItemPred withoutToMove
                    |> Maybe.map (\i -> List.Extra.splitAt (i + 1) withoutToMove)
            of
                Nothing ->
                    list

                Just ( left, right ) ->
                    left ++ toMove :: right


view : Model -> Html Msg
view m =
    let
        preventDefault bool =
            JD.map (\msg -> ( msg, bool ))

        dragEvents : { itemId : String, domId : String } -> List (Attribute Msg)
        dragEvents meta =
            [ draggable "true"
            , Html.Events.preventDefaultOn "dragstart"
                (JD.map (OnDragStart meta) domeEvDecoder
                    |> preventDefault True
                )
            ]

        dropEvents : { itemId : String, domId : String } -> List (Attribute Msg)
        dropEvents meta =
            [ Html.Events.onMouseEnter (OnDragOver meta)
            ]

        iv item =
            let
                elId =
                    itemToDomId item

                meta =
                    { itemId = item.id, domId = elId }
            in
            case m.di of
                Just di ->
                    if di.dragId == item.id then
                        div
                            [ class "ph1 bb b--black-10 pointer no-selection"
                            , class "o-10"
                            , hid elId
                            ]
                            [ text item.title ]

                    else
                        div
                            ([ class "ph1 bb b--black-10 pointer no-selection"
                             , hid elId
                             ]
                                ++ dropEvents meta
                            )
                            [ text item.title ]

                Nothing ->
                    div
                        ([ class "ph1 bb b--black-10 pointer no-selection"
                         , hid elId
                         ]
                            ++ dragEvents meta
                        )
                        [ text item.title ]

        sortedItems =
            case m.di of
                Just di ->
                    if Dict.size di.elDict == List.length m.items then
                        dndSortItems di m.items

                    else
                        m.items

                Nothing ->
                    m.items
    in
    div [ class "pv3 ph5 measure-narrow f3 lh-copy" ]
        [ div [ class "pv2" ] [ text "OZ Outliner" ]
        , div [] (List.map iv sortedItems)
        ]


hid =
    Html.Attributes.id



-- Main
--main : Program Flags Model Msg


main =
    let
        defaultConfig : TimeTravel.Browser.TimeTravelConfig
        defaultConfig =
            TimeTravel.Browser.defaultConfig
    in
    TimeTravel.Browser.element
        Debug.toString
        Debug.toString
        { defaultConfig | startToLeft = False, startMinimized = True }
        --Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
