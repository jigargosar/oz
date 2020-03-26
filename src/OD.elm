port module OD exposing (main)

import Browser
import Browser.Dom as Dom
import CollapseState
import Html exposing (Html, div, i, input, span, text)
import Html.Attributes exposing (class, placeholder, tabindex, value)
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



-- Query


type Query
    = Query String



-- Model


type Model
    = Model State String Seed


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
            ( Model (NoState newOD) "" seed
            , focusPrimary
            )

        Ok (Just od) ->
            ( Model (NoState od) "" (Random.initialSeed flags.now)
            , focusPrimary
            )

        Err err ->
            Debug.todo (JD.errorToString err)


stepSetState : Generator State -> Model -> Model
stepSetState genF (Model _ qs seed0) =
    let
        ( state, seed ) =
            Random.step genF seed0
    in
    Model state qs seed


setState : State -> Model -> Model
setState state (Model _ qs seed) =
    Model state qs seed


setNoState : OD -> Model -> Model
setNoState =
    NoState >> setState



-- STATE


type State
    = Edit String OD
    | NoState OD



-- DOM FOCUS


focusPrimary : Cmd Msg
focusPrimary =
    Dom.focus "primary-focus-node" |> Task.attempt OnFocusResult


focusSearch : Cmd Msg
focusSearch =
    Dom.focus "search-input" |> Task.attempt OnFocusResult



-- Update


type alias Ret =
    ( Model, Cmd Msg )


type Msg
    = NoOp
    | OnFocusResult (Result Dom.Error ())
    | QueryChanged String
    | OnQueryEnter
    | OnQueryShiftEnter
    | SearchForward
    | SearchBackward
    | FocusSearch
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


aroundUpdate : Msg -> Model -> ( Model, Cmd Msg )
aroundUpdate msg ((Model oldState _ _) as model) =
    let
        ( newModel, cmd ) =
            update msg model

        (Model newState _ _) =
            newModel
    in
    ( newModel
    , Cmd.batch
        [ cmd
        , cacheState oldState newState
        ]
    )


cacheState : State -> State -> Cmd msg
cacheState o n =
    let
        cacheODCmd : OD -> Cmd msg
        cacheODCmd od =
            cacheKV ( "od", odEncoder od )

        stateToOD state =
            case state of
                Edit _ od ->
                    od

                NoState od ->
                    od
    in
    cmdIf (neqBy stateToOD o n) (cacheODCmd (stateToOD n))


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        OnFocusResult (Ok ()) ->
            ( model, Cmd.none )

        OnFocusResult (Err (Dom.NotFound domId)) ->
            Debug.todo ("focus failed on: " ++ domId)

        FocusSearch ->
            ( model, focusSearch )

        QueryChanged nqs ->
            ( case model of
                Model s _ seed ->
                    Model s nqs seed
            , Cmd.none
            )

        TitleChanged changedTitle ->
            ( case model of
                Model (Edit _ od) _ _ ->
                    setState (Edit changedTitle od) model

                _ ->
                    model
            , Cmd.none
            )

        OnEnter ->
            onEnter model

        OnQueryEnter ->
            mmQOD searchForward model

        OnQueryShiftEnter ->
            mmQOD searchBackward model

        SearchForward ->
            mmQODFocus searchForward model

        SearchBackward ->
            mmQODFocus searchBackward model

        OnCursorUp ->
            mmODFocus tryBackwardVisible model

        OnCursorDown ->
            mmODFocus tryForwardVisible model

        OnCursorLeft ->
            mmODFocus (firstOf [ tryCollapse, tryUp, tryLeft ]) model

        OnCursorRight ->
            mmODFocus (firstOf [ tryExpand, tryForwardVisible ]) model

        ZoomIn ->
            mmODFocus (firstOf [ tryZoomIn, tryZoomInParent ]) model

        ZoomOut ->
            mmODFocus tryZoomOut model

        Indent ->
            mmODFocus tryIndent model

        UnIndent ->
            mmODFocus tryUnIndent model


mmODFocus : (OD -> Maybe OD) -> Model -> Ret
mmODFocus func model =
    case model of
        Model (NoState od) _ _ ->
            case func od of
                Just nod ->
                    ( setNoState nod model, focusPrimary )

                _ ->
                    ( model, Cmd.none )

        Model (Edit title od) _ _ ->
            case func od of
                Just nod ->
                    if odEqById od nod then
                        ( setState (Edit title nod) model, focusPrimary )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


mmQOD : (Query -> OD -> Maybe OD) -> Model -> Ret
mmQOD func model =
    case model of
        Model (NoState od) q _ ->
            case func (Query q) od of
                Just nod ->
                    ( setNoState nod model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


mmQODFocus : (Query -> OD -> Maybe OD) -> Model -> Ret
mmQODFocus func model =
    case model of
        Model (NoState od) q _ ->
            case func (Query q) od of
                Just nod ->
                    ( setNoState nod model, focusPrimary )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


onEnter : Model -> Ret
onEnter ((Model state _ _) as model) =
    let
        initEditState : OD -> State
        initEditState od =
            Edit (odTitle od) od
    in
    case state of
        NoState od ->
            ( setState (initEditState od) model
            , focusPrimary
            )

        Edit unsafeTitle od ->
            case nonBlank unsafeTitle of
                Just title ->
                    let
                        setTitleThenAddNew : Generator State
                        setTitleThenAddNew =
                            odSetTitle title od
                                |> addNew
                                |> Random.map initEditState
                    in
                    ( stepSetState setTitleThenAddNew model
                    , focusPrimary
                    )

                Nothing ->
                    let
                        removeLeafOrSetEmptyTitle : OD
                        removeLeafOrSetEmptyTitle =
                            case removeLeaf od of
                                Just nod ->
                                    nod

                                Nothing ->
                                    odSetTitle "" od
                    in
                    ( setNoState removeLeafOrSetEmptyTitle model
                    , focusPrimary
                    )


searchForward : Query -> OD -> Maybe OD
searchForward =
    matches >> findNextWrap


searchBackward : Query -> OD -> Maybe OD
searchBackward =
    matches >> findPrevWrap


matches : Query -> OD -> Bool
matches (Query qs) od =
    String.contains (String.toLower qs) (String.toLower (odTitle od))


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- VIEW


type alias HM =
    Html Msg


type alias LHM =
    List (Html Msg)


view : Model -> Html Msg
view (Model state qs _) =
    div []
        [ div [ class "center measure-wide" ]
            [ div [ class "pa1 f4 lh-title" ] [ text "OZ OUTLINING V2" ]
            , viewSearchQuery qs
            , viewOD qs state
            ]
        ]


viewSearchQuery : String -> HM
viewSearchQuery qs =
    div [ class "pv1 flex" ]
        [ input
            [ Html.Attributes.id "search-input"
            , class "flex-auto"
            , value qs
            , onInput QueryChanged
            , placeholder "Search..."
            , onKeyDownHelp
                [ ( KeyEvent.hot "Enter", OnQueryEnter )
                , ( KeyEvent.shift "Enter", OnQueryShiftEnter )
                ]
            ]
            []
        ]



-- OD VM View


type IV
    = IVEdit String
    | IVShow String
    | IVFocused String


type TV
    = TVLeaf IV
    | TVCollapsed IV
    | TVExpanded IV (List TV)


type ZV
    = ZV String


viewOD : String -> State -> HM
viewOD qs state =
    let
        stateFu =
            case state of
                Edit title od ->
                    ( IVEdit title, od )

                NoState od ->
                    ( IVFocused (odTitle od), od )
    in
    let
        ( itemToIV, od ) =
            stateFu

        tvl =
            odToTVL itemToIV od
    in
    div []
        [ viewZoomCrumbs (odToZVL od)
        , div [] (List.map (viewTV (Query qs)) tvl)
        ]


viewZoomCrumbs : List ZV -> HM
viewZoomCrumbs zvl =
    let
        viewZV (ZV title) =
            div [ class "pa1" ] [ displayTitleEl title ]

        viewSep =
            i [ class "material-icons md-18 light-silver" ]
                [ text "chevron_right" ]
    in
    div [ class "flex items-center" ]
        (div [ class "pa1" ] [ displayTitleEl "Home" ]
            :: List.map viewZV zvl
            |> List.intersperse viewSep
        )


viewTV : Query -> TV -> HM
viewTV query tv =
    let
        ( iv2, collapseState, tvs2 ) =
            case tv of
                TVLeaf iv ->
                    ( iv, CollapseState.NoChildren, [] )

                TVCollapsed iv ->
                    ( iv, CollapseState.Collapsed, [] )

                TVExpanded iv tvs ->
                    ( iv, CollapseState.Expanded, tvs )

        iconName =
            case collapseState of
                CollapseState.NoChildren ->
                    "chevron_right"

                CollapseState.Expanded ->
                    "expand_more"

                CollapseState.Collapsed ->
                    "chevron_right"
    in
    treeContainer
        [ div [ class "flex" ]
            [ i
                [ class "self-start pt2 ph1 material-icons md-24 light-silver"
                , classIf (collapseState == CollapseState.NoChildren) "o-0"
                ]
                [ text iconName ]
            , div [ class "flex-auto flex f4 lh-title" ]
                [ viewIV query iv2
                ]
            ]
        , treeChildrenContainer <|
            List.map (viewTV query) tvs2
        ]


viewIV : Query -> IV -> HM
viewIV (Query _) iv =
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

        IVFocused title ->
            let
                key =
                    KeyEvent.hot

                shift =
                    KeyEvent.shift

                any =
                    anyPass
            in
            div
                [ Html.Attributes.id "primary-focus-node"
                , class "flex-auto pa1 bg-lightest-blue"
                , tabindex 0
                , onKeyDownHelp
                    [ ( key "Enter", OnEnter )
                    , ( any [ key "ArrowUp", key "k" ], OnCursorUp )
                    , ( any [ key "ArrowDown", key "j" ], OnCursorDown )
                    , ( any [ key "ArrowLeft", key "h" ], OnCursorLeft )
                    , ( any [ key "ArrowRight", key "l" ], OnCursorRight )
                    , ( key "Tab", Indent )
                    , ( shift "Tab", UnIndent )
                    , ( key "n", SearchForward )
                    , ( shift "N", SearchBackward )
                    , ( shift "ArrowRight", ZoomIn )
                    , ( shift "ArrowLeft", ZoomOut )
                    , ( key "/", FocusSearch )
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


onKeyDownHelp : List ( KeyEvent.KeyEvent -> Bool, a ) -> Html.Attribute a
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


odEqById : OD -> OD -> Bool
odEqById =
    eqBy odId


odId : OD -> Id
odId (OD _ _ (LTR _ (T (Item id _ _) _) _)) =
    id


odTitle : OD -> String
odTitle (OD _ _ (LTR _ (T (Item _ _ title) _) _)) =
    title


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


odSetTitle : String -> OD -> OD
odSetTitle title (OD pcs cs (LTR l (T (Item id collapsed _) ts) r)) =
    OD pcs cs (LTR l (T (Item id collapsed title) ts) r)


findNext : (OD -> Bool) -> OD -> Maybe OD
findNext pred =
    findX pred tryForward


findNextWrap : (OD -> Bool) -> OD -> Maybe OD
findNextWrap pred =
    firstOf [ findNext pred, firstRoot >> findNext pred ]


findPrevWrap : (OD -> Bool) -> OD -> Maybe OD
findPrevWrap pred =
    firstOf [ findPrev pred, lastDescendentOfLastRoot >> findPrev pred ]


lastDescendentOfLastRoot : OD -> OD
lastDescendentOfLastRoot =
    lastRoot >> lastDescendent


findPrev : (OD -> Bool) -> OD -> Maybe OD
findPrev pred =
    findX pred tryBackward


tryForwardVisible : OD -> Maybe OD
tryForwardVisible =
    firstOf [ tryDownVisible, tryRight, tryRightOfAncestor ]


tryForward : OD -> Maybe OD
tryForward =
    firstOf [ tryDown, tryRight, tryRightOfAncestor ]


tryBackwardVisible : OD -> Maybe OD
tryBackwardVisible =
    firstOf [ tryLeft >> Maybe.map lastDescendentVisible, tryUp ]


tryBackward : OD -> Maybe OD
tryBackward =
    firstOf [ tryLeft >> Maybe.map lastDescendent, tryUp ]


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


tryDownVisible : OD -> Maybe OD
tryDownVisible (OD pcs cs (LTR l t r)) =
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


tryDown : OD -> Maybe OD
tryDown (OD pcs cs (LTR l (T item ts) r)) =
    case ts of
        [] ->
            Nothing

        first :: rest ->
            LTR [] first rest
                |> OD pcs (Crumb l item r :: cs)
                |> Just


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


firstRoot : OD -> OD
firstRoot =
    root >> applyWhileJust tryLeft


lastRoot : OD -> OD
lastRoot =
    root >> applyWhileJust tryRight


root : OD -> OD
root =
    applyWhileJust tryUp


lastDescendent : OD -> OD
lastDescendent od =
    case tryDown od of
        Just cod ->
            lastDescendent (applyWhileJust tryRight cod)

        Nothing ->
            od


lastDescendentVisible : OD -> OD
lastDescendentVisible od =
    case tryDownVisible od of
        Just cod ->
            lastDescendent (applyWhileJust tryRight cod)

        Nothing ->
            od


tryIndent : OD -> Maybe OD
tryIndent (OD pcs cs (LTR l t r)) =
    case l of
        [] ->
            Nothing

        (T item ts) :: rest ->
            LTR (List.reverse ts) t []
                |> OD pcs (Crumb rest item r :: cs)
                |> Just


tryUnIndent : OD -> Maybe OD
tryUnIndent (OD pcs cs (LTR l t r)) =
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
            OD rest (cs ++ [ first ]) ltr |> Just


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



-- OD to VM


odToTVL : IV -> OD -> List TV
odToTVL iv (OD _ cs (LTR l (T ((Item _ collapsed _) as item) ts) r)) =
    let
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


odToZVL : OD -> List ZV
odToZVL (OD pcs _ _) =
    let
        cToZV (Crumb _ (Item _ _ title) _) =
            ZV title
    in
    List.foldl (cToZV >> cons) [] pcs


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
