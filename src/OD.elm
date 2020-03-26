port module OD exposing (main)

import Browser
import Browser.Dom as Dom
import Html exposing (Html, div, i, input, span, text)
import Html.Attributes exposing (class, tabindex, value)
import Html.Events exposing (onInput)
import ItemId exposing (ItemId)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import KeyEvent
import Random exposing (Generator, Seed)
import Task
import Utils exposing (..)


port cacheKV : ( String, Value ) -> Cmd msg



-- Main


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = aroundUpdate
        , subscriptions = subscriptions
        }



-- Model


type Model
    = Model State Seed


type State
    = Edit String OD
    | NoEdit OD


type StateType
    = EditType
    | NoEditType


type alias Flags =
    { now : Int, od : Value }


init : Flags -> ( Model, Cmd Msg )
init flags =
    case JD.decodeValue (JD.nullable odDecoder) flags.od of
        Ok Nothing ->
            let
                ( newOD, seed ) =
                    Random.step new (Random.initialSeed flags.now)
            in
            ( Model (NoEdit newOD) seed
            , Dom.focus "primary-focus-node" |> Task.attempt OnFocusResult
            )

        Ok (Just od) ->
            ( Model (NoEdit od) (Random.initialSeed flags.now)
            , Dom.focus "primary-focus-node" |> Task.attempt OnFocusResult
            )

        Err err ->
            Debug.todo (JD.errorToString err)



-- Update


type Msg
    = NoOp
    | OnFocusResult (Result Dom.Error ())
    | OnEnter
    | TitleChanged String
    | OnCursorUp
    | OnCursorDown
    | OnCursorLeft
    | OnCursorRight
    | Indent
    | UnIndent
    | ZoomIn
    | ZoomOut


odOf : State -> OD
odOf state =
    case state of
        Edit _ od ->
            od

        NoEdit od ->
            od


typeOf : State -> StateType
typeOf state =
    case state of
        Edit _ _ ->
            EditType

        NoEdit _ ->
            NoEditType


aroundUpdate : Msg -> Model -> ( Model, Cmd Msg )
aroundUpdate msg ((Model oldState _) as model) =
    let
        newModel =
            update msg model

        (Model newState _) =
            newModel

        docChanged =
            neqBy odOf oldState newState

        stateSwitched =
            neqBy typeOf oldState newState
    in
    ( newModel
    , Cmd.batch
        [ cmdIf (docChanged || stateSwitched)
            (Dom.focus "primary-focus-node" |> Task.attempt OnFocusResult)
        , cmdIf docChanged (cacheODCmd (odOf newState))
        ]
    )


cacheODCmd : OD -> Cmd msg
cacheODCmd od =
    cacheKV ( "od", odEncoder od )


update : Msg -> Model -> Model
update message ((Model state seed) as model) =
    case message of
        NoOp ->
            model

        OnFocusResult (Ok ()) ->
            model

        OnFocusResult (Err (Dom.NotFound domId)) ->
            Debug.todo ("focus failed on: " ++ domId)

        OnEnter ->
            case state of
                NoEdit od ->
                    case itemOf od of
                        Item _ _ title ->
                            Model (Edit title od) seed

                Edit title od ->
                    case nonBlank title of
                        Just nbTitle ->
                            Random.step (addNew (odSetTitle nbTitle od)) seed
                                |> uncurry (Edit "" >> Model)

                        Nothing ->
                            case removeLeaf od of
                                Just newOd ->
                                    Model (NoEdit newOd) seed

                                Nothing ->
                                    Model (NoEdit (odSetTitle "" od)) seed

        TitleChanged changedTitle ->
            case state of
                Edit _ od ->
                    Model (Edit changedTitle od) seed

                NoEdit _ ->
                    model

        OnCursorUp ->
            case state of
                NoEdit od ->
                    case firstOf [ tryLeft, tryUp ] od of
                        Just newOD ->
                            Model (NoEdit newOD) seed

                        Nothing ->
                            model

                Edit _ _ ->
                    model

        OnCursorDown ->
            case state of
                NoEdit od ->
                    case firstOf [ tryDown, tryRight, tryRightOfAncestor ] od of
                        Just newOD ->
                            Model (NoEdit newOD) seed

                        Nothing ->
                            model

                Edit _ _ ->
                    model

        OnCursorLeft ->
            case state of
                NoEdit od ->
                    case firstOf [ tryCollapse, tryUp, tryLeft ] od of
                        Just newOD ->
                            Model (NoEdit newOD) seed

                        Nothing ->
                            model

                Edit _ _ ->
                    model

        OnCursorRight ->
            case state of
                NoEdit od ->
                    case firstOf [ tryExpand, tryDown, tryRight, tryRightOfAncestor ] od of
                        Just newOD ->
                            Model (NoEdit newOD) seed

                        Nothing ->
                            model

                Edit _ _ ->
                    model

        Indent ->
            let
                maybeNewState =
                    case state of
                        NoEdit od ->
                            indent od |> Maybe.map NoEdit

                        Edit t od ->
                            indent od |> Maybe.map (Edit t)
            in
            case maybeNewState of
                Just newState ->
                    Model newState seed

                Nothing ->
                    model

        UnIndent ->
            let
                maybeNewState =
                    case state of
                        NoEdit od ->
                            unIndent od |> Maybe.map NoEdit

                        Edit t od ->
                            unIndent od |> Maybe.map (Edit t)
            in
            case maybeNewState of
                Just newState ->
                    Model newState seed

                Nothing ->
                    model

        ZoomIn ->
            case state of
                NoEdit od ->
                    case firstOf [ tryZoomIn, tryZoomInParent ] od of
                        Just newOD ->
                            Model (NoEdit newOD) seed

                        Nothing ->
                            model

                Edit _ _ ->
                    model

        ZoomOut ->
            case state of
                NoEdit od ->
                    case firstOf [ tryZoomOut ] od of
                        Just newOD ->
                            Model (NoEdit newOD) seed

                        Nothing ->
                            model

                Edit _ _ ->
                    model


tryExpand : OD -> Maybe OD
tryExpand (OD pcs cs (LTR l (T item ts) r)) =
    case ( ts, itemExpanded item ) of
        ( _ :: _, False ) ->
            LTR l (T (setItemExpanded True item) ts) r
                |> OD pcs cs
                |> Just

        _ ->
            Nothing


tryCollapse : OD -> Maybe OD
tryCollapse (OD pcs cs (LTR l (T item ts) r)) =
    case ( ts, itemCollapsed item ) of
        ( _ :: _, False ) ->
            LTR l (T (setItemCollapsed True item) ts) r
                |> OD pcs cs
                |> Just

        _ ->
            Nothing


tryRight : OD -> Maybe OD
tryRight (OD pcs cs (LTR l t r)) =
    case r of
        first :: rest ->
            LTR (t :: l) first rest
                |> OD pcs cs
                |> Just

        [] ->
            Nothing


tryLeft : OD -> Maybe OD
tryLeft (OD pcs cs (LTR l t r)) =
    case l of
        first :: rest ->
            LTR rest first (t :: r)
                |> OD pcs cs
                |> Just

        [] ->
            Nothing


tryUp : OD -> Maybe OD
tryUp (OD pcs cs (LTR l t r)) =
    case cs of
        (Crumb cl item cr) :: rest ->
            LTR cl (T item (List.reverse l ++ t :: r)) cr
                |> OD pcs rest
                |> Just

        [] ->
            Nothing


tryDown : OD -> Maybe OD
tryDown (OD pcs cs (LTR l t r)) =
    visibleChildren t
        |> Maybe.andThen
            (\(T item ts) ->
                case ts of
                    [] ->
                        Nothing

                    first :: rest ->
                        LTR [] first rest
                            |> OD pcs (Crumb l item r :: cs)
                            |> Just
            )


tryRightOfAncestor : OD -> Maybe OD
tryRightOfAncestor (OD pcs cs (LTR l t r)) =
    case cs of
        (Crumb cl item (crFirst :: crRest)) :: rest ->
            LTR (T item (List.reverse l ++ t :: r) :: cl) crFirst crRest
                |> OD pcs rest
                |> Just

        (Crumb cl item []) :: rest ->
            case
                LTR cl (T item (List.reverse l ++ t :: r)) []
                    |> OD pcs rest
            of
                parentOd ->
                    tryRightOfAncestor parentOd

        [] ->
            Nothing


itemOf : OD -> Item
itemOf (OD _ _ (LTR _ (T item _) _)) =
    item



--idOfOd : OD -> Id
--idOfOd =
--    itemOf >> idOf


odSetTitle : String -> OD -> OD
odSetTitle title (OD pcs cs (LTR l (T (Item id collapsed _) ts) r)) =
    OD pcs cs (LTR l (T (Item id collapsed title) ts) r)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- VIEW


type alias HM =
    Html Msg


type alias LHM =
    List (Html Msg)


view : Model -> Html Msg
view (Model state _) =
    div []
        [ div [ class "center measure-wide" ]
            [ div [ class "pa1 f4 lh-title" ] [ text "OZ OUTLINING V2" ]
            , viewOD2 state
            ]
        ]



-- OUTLINE DOC


type OD
    = OD (List Crumb) (List Crumb) LTR


odEncoder : OD -> Value
odEncoder (OD pcs cs (LTR l t r)) =
    JE.object
        [ ( "pcs", JE.list crumbEncoder pcs )
        , ( "cs", JE.list crumbEncoder cs )
        , ( "l", JE.list treeEncoder l )
        , ( "t", treeEncoder t )
        , ( "r", JE.list treeEncoder r )
        ]


odDecoder : Decoder OD
odDecoder =
    JD.succeed OD
        |> requiredList "pcs" crumbDecoder
        |> requiredList "cs" crumbDecoder
        |> JD.map2 (|>)
            (JD.succeed LTR
                |> requiredList "l" treeDecoder
                |> required "t" treeDecoder
                |> requiredList "r" treeDecoder
            )


new : Generator OD
new =
    idGen |> Random.map newHelp


newHelp : ItemId -> OD
newHelp id =
    OD [] [] (LTR [] (treeFromId id) [])


addNew : OD -> Generator OD
addNew od =
    idGen |> Random.map (flip addNewHelp od)


addNewHelp : ItemId -> OD -> OD
addNewHelp id (OD pcs cs (LTR l t r)) =
    let
        newT =
            treeFromId id
    in
    if hasVisibleChildren t then
        -- prepend child
        let
            (T item children) =
                t

            newCrumb =
                Crumb l item r

            newLTR =
                LTR [] newT children
        in
        OD pcs (newCrumb :: cs) newLTR

    else
        -- insertAfter
        OD pcs cs (LTR (t :: l) newT r)


indent : OD -> Maybe OD
indent (OD pcs cs (LTR l t r)) =
    case l of
        [] ->
            Nothing

        (T item ts) :: rest ->
            LTR (List.reverse ts) t []
                |> OD pcs (Crumb rest item r :: cs)
                |> Just


unIndent : OD -> Maybe OD
unIndent (OD pcs cs (LTR l t r)) =
    case cs of
        [] ->
            Nothing

        (Crumb cl item cr) :: rest ->
            LTR (T item (List.reverse l ++ r) :: cl) t cr
                |> OD pcs rest
                |> Just


tryZoomInParent : OD -> Maybe OD
tryZoomInParent (OD pcs cs ltr) =
    case cs of
        [] ->
            Nothing

        _ :: _ ->
            OD (cs ++ pcs) [] ltr
                |> Just


tryZoomIn : OD -> Maybe OD
tryZoomIn (OD pcs cs (LTR l (T item ts) r)) =
    case ts of
        [] ->
            Nothing

        first :: rest ->
            LTR [] first rest
                |> OD ((Crumb l item r :: cs) ++ pcs) []
                |> Just


tryZoomOut : OD -> Maybe OD
tryZoomOut (OD pcs cs ltr) =
    case pcs of
        [] ->
            Nothing

        first :: rest ->
            OD rest (first :: cs) ltr |> Just


removeLeaf : OD -> Maybe OD
removeLeaf ((OD _ _ (LTR _ t _)) as od) =
    if hasChildren t then
        Nothing

    else
        removeGoLeftOrRightOrUp od


removeGoLeftOrRightOrUp : OD -> Maybe OD
removeGoLeftOrRightOrUp (OD pcs cs (LTR l _ r)) =
    case ( l, r, cs ) of
        ( first :: rest, _, _ ) ->
            LTR rest first r
                |> OD pcs cs
                |> Just

        ( _, first :: rest, _ ) ->
            LTR l first rest
                |> OD pcs cs
                |> Just

        ( _, _, (Crumb cl item cr) :: rest ) ->
            LTR cl (T item []) cr
                |> OD pcs rest
                |> Just

        _ ->
            Nothing



-- OD VM


type IV
    = IVEdit String
    | IVShow String
    | IVShowFocused String


type TV
    = TVLeaf IV
    | TVCollapsed IV
    | TVExpanded IV (List TV)


odToTVL : (Item -> IV) -> OD -> List TV
odToTVL itemToIV (OD _ cs (LTR l (T ((Item _ collapsed _) as item) ts) r)) =
    let
        iv =
            itemToIV item

        tv =
            case ( ts, collapsed ) of
                ( [], _ ) ->
                    TVLeaf iv

                ( _, True ) ->
                    TVCollapsed iv

                ( _, False ) ->
                    TVExpanded iv (List.map toTV ts)
    in
    List.foldl crumbToLVR (LVR l tv r) cs
        |> lvrToTVL


type LVR
    = LVR (List T) TV (List T)


crumbToLVR : Crumb -> LVR -> LVR
crumbToLVR (Crumb l (Item _ _ title) r) lvr =
    LVR l (TVExpanded (IVShow title) (lvrToTVL lvr)) r


lvrToTVL : LVR -> List TV
lvrToTVL (LVR l tv r) =
    (l |> List.reverse |> List.map toTV) ++ tv :: (r |> List.map toTV)


toTV : T -> TV
toTV (T (Item _ collapsed title) ts) =
    case ( ts, collapsed ) of
        ( [], _ ) ->
            TVLeaf (IVShow title)

        ( _, True ) ->
            TVCollapsed (IVShow title)

        ( _, False ) ->
            TVExpanded (IVShow title) (List.map toTV ts)



-- OD VM View


viewOD2 : State -> HM
viewOD2 state =
    case state of
        Edit title od ->
            treeChildrenContainer
                (List.map viewTV (odToTVL (always (IVEdit title)) od))

        NoEdit od ->
            treeChildrenContainer
                (List.map viewTV (odToTVL (\(Item _ _ title) -> IVShowFocused title) od))


viewLine : Bool -> String -> IV -> HM
viewLine hideIcon iconName iv =
    div [ class "flex" ]
        [ i
            [ class "self-start pt2 ph1 material-icons md-24 light-silver"
            , classIf hideIcon "o-0"
            ]
            [ text iconName ]
        , div [ class "flex-auto flex f4 lh-title" ] [ viewIV iv ]
        ]


viewTV : TV -> HM
viewTV tv =
    treeContainer <|
        case tv of
            TVLeaf iv ->
                [ viewLine True "chevron_right" iv ]

            TVCollapsed iv ->
                [ viewLine False "chevron_right" iv ]

            TVExpanded iv tvs ->
                [ viewLine False "expand_more" iv
                , treeChildrenContainer <|
                    List.map viewTV tvs
                ]


viewIV : IV -> HM
viewIV iv =
    case iv of
        IVEdit title ->
            input
                [ Html.Attributes.id "primary-focus-node"
                , class "flex-auto pa1 bn bw0 ma0"
                , tabindex 0
                , value title
                , onInput TitleChanged
                , onKeyDownHelp
                    [ ( KeyEvent.hot "Enter", OnEnter )
                    , ( KeyEvent.hot "Tab", Indent )
                    , ( KeyEvent.shift "Tab", UnIndent )
                    ]
                ]
                []

        IVShow title ->
            div [ class "flex-auto pa1" ] [ displayTitleEl title ]

        IVShowFocused title ->
            div
                [ Html.Attributes.id "primary-focus-node"
                , class "flex-auto pa1"
                , tabindex 0
                , onKeyDownHelp
                    [ ( KeyEvent.hot "Enter", OnEnter )
                    , ( KeyEvent.hot "ArrowUp", OnCursorUp )
                    , ( KeyEvent.hot "ArrowDown", OnCursorDown )
                    , ( KeyEvent.hot "ArrowLeft", OnCursorLeft )
                    , ( KeyEvent.hot "ArrowRight", OnCursorRight )
                    , ( KeyEvent.hot "Tab", Indent )
                    , ( KeyEvent.shift "Tab", UnIndent )
                    , ( KeyEvent.shift "ArrowRight", ZoomIn )
                    , ( KeyEvent.shift "ArrowLeft", ZoomOut )
                    ]
                ]
                [ displayTitleEl title ]


treeChildrenContainer : LHM -> HM
treeChildrenContainer =
    div [ class "pl4" ]


treeContainer : LHM -> HM
treeContainer =
    div []


displayTitleEl : String -> HM
displayTitleEl title =
    case nonBlank title of
        Just nb ->
            span [ class "" ] [ text nb ]

        Nothing ->
            span [ class "silver" ] [ text "Untitled" ]


onKeyDownHelp conditions =
    Html.Events.preventDefaultOn "keydown"
        (KeyEvent.decoder
            |> JD.andThen
                (\ke ->
                    condAlways conditions ke
                        |> Maybe.map (\msg -> JD.succeed ( msg, True ))
                        |> Maybe.withDefault (JD.fail "Not interested")
                )
        )



-- CRUMB


type Crumb
    = Crumb (List T) Item (List T)


crumbEncoder : Crumb -> Value
crumbEncoder (Crumb l item r) =
    JE.object
        [ ( "l", JE.list treeEncoder l )
        , ( "item", itemEncoder item )
        , ( "r", JE.list treeEncoder r )
        ]


crumbDecoder : Decoder Crumb
crumbDecoder =
    JD.succeed Crumb
        |> requiredList "l" treeDecoder
        |> required "item" itemDecoder
        |> requiredList "r" treeDecoder



-- TREE CHILDREN ZIPPER


type LTR
    = LTR (List T) T (List T)


type T
    = T Item (List T)


treeEncoder : T -> Value
treeEncoder (T item ts) =
    JE.object
        [ ( "item", itemEncoder item )
        , ( "ts", JE.list treeEncoder ts )
        ]


treeDecoder : Decoder T
treeDecoder =
    JD.succeed T
        |> required "item" itemDecoder
        |> requiredList "ts" (JD.lazy (\_ -> treeDecoder))


treeFromId : Id -> T
treeFromId id =
    T (itemFromId id) []


hasChildren : T -> Bool
hasChildren (T _ ts) =
    not (List.isEmpty ts)


hasVisibleChildren : T -> Bool
hasVisibleChildren ((T item _) as t) =
    hasChildren t && itemExpanded item


visibleChildren : T -> Maybe T
visibleChildren t =
    if hasVisibleChildren t then
        Just t

    else
        Nothing



--expandTree : T -> Maybe T
--expandTree ((T item ts) as t) =
--    case ( ts, itemCollapsed item ) of
--        ( _ :: _, True ) ->
--            T (setItemCollapsed False item) ts
--                |> Just
--
--        _ ->
--            Nothing
-- ITEM


type Item
    = Item Id Bool String


itemEncoder : Item -> Value
itemEncoder (Item id collapsed title) =
    JE.object
        [ ( "id", ItemId.itemIdEncoder id )
        , ( "collapsed", JE.bool collapsed )
        , ( "title", JE.string title )
        ]


itemDecoder : Decoder Item
itemDecoder =
    JD.succeed Item
        |> required "id" ItemId.itemIdDecoder
        |> requiredBool "collapsed"
        |> requiredString "title"



--idOf : Item -> Id
--idOf (Item id _ _) =
--    id


itemFromId : Id -> Item
itemFromId id =
    Item id False ""


itemCollapsed : Item -> Bool
itemCollapsed (Item _ c _) =
    c


itemExpanded : Item -> Bool
itemExpanded =
    itemCollapsed >> not


setItemCollapsed : Bool -> Item -> Item
setItemCollapsed collapsed (Item id _ title) =
    Item id collapsed title


setItemExpanded : Bool -> Item -> Item
setItemExpanded =
    not >> setItemCollapsed



--itemDisplayTitle : Item -> String
--itemDisplayTitle (Item _ _ ti) =
--    case nonBlank ti of
--        Just title ->
--            title
--
--        Nothing ->
--            "Untitled"
--
-- Id


type alias Id =
    ItemId


idGen : Generator Id
idGen =
    ItemId.generator
