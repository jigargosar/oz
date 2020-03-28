port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html exposing (Html, button, div, i, input, span, text)
import Html.Attributes exposing (accesskey, class, placeholder, tabindex, value)
import Html.Events exposing (onClick, onInput)
import ItemId exposing (ItemId)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Extra
import Json.Encode as JE exposing (Value)
import KeyEvent exposing (KeyEvent)
import Random exposing (Generator, Seed)
import Task
import Time exposing (Posix)
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
    | Download
    | DownloadNow Posix
    | Upload
    | GotFile File
    | GotFileString String
    | OnFocusResult (Result Dom.Error ())
    | QueryChanged String
    | TitleChanged String
    | OnEsc
    | OnEscQ
    | OnEnter
    | IdClicked Id
    | OnQueryEnter
    | OnQueryShiftEnter
    | SearchForward
    | SearchBackward
    | FocusSearch
    | OnCursorUp
    | OnCursorDown
    | OnCursorUpRelocate
    | OnCursorDownRelocate
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


jsonMIMEType =
    "application/json"


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        Download ->
            ( model, Time.now |> Task.perform DownloadNow )

        DownloadNow now ->
            let
                formattedNow =
                    Time.posixToMillis now |> String.fromInt

                fileName =
                    "oz_" ++ formattedNow ++ ".json"

                od2 =
                    case model of
                        Model (NoState od) _ _ ->
                            od

                        Model (Edit _ od) _ _ ->
                            od

                fileContent =
                    odEncoder od2
                        |> JE.encode 2
            in
            ( model, Download.string fileName jsonMIMEType fileContent )

        Upload ->
            ( model, Select.file [ jsonMIMEType ] GotFile )

        GotFile file ->
            ( model, File.toString file |> Task.perform GotFileString )

        GotFileString fs ->
            case JD.decodeString odDecoder fs of
                Ok od ->
                    ( setNoState od model, focusPrimary )

                Err e ->
                    let
                        _ =
                            Debug.log "GotFileString" (JD.errorToString e)
                    in
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

        OnEsc ->
            ( case model of
                Model (NoState _) _ _ ->
                    setQ "" model

                Model (Edit _ od) _ _ ->
                    setNoState (ignoreNothing removeBlankLeaf od) model
            , focusPrimary
            )

        OnEscQ ->
            ( setQ "" model, focusPrimary )

        OnEnter ->
            onEnter model

        IdClicked id ->
            mmODFocus (findId id) model

        OnQueryEnter ->
            mmQOD searchForward model

        OnQueryShiftEnter ->
            mmQOD searchBackward model

        SearchForward ->
            mmQODFocus searchForward model

        SearchBackward ->
            mmQODFocus searchBackward model

        OnCursorUp ->
            mmODFocus tryBackward model

        OnCursorUpRelocate ->
            mmODFocus relocateBefore model

        OnCursorDown ->
            mmODFocus tryForward model

        OnCursorDownRelocate ->
            mmODFocus relocateAfter model

        OnCursorLeft ->
            mmODFocus (firstOf [ tryCollapse, parent, tryLeft ]) model

        OnCursorRight ->
            mmODFocus (firstOf [ tryExpand, tryForward ]) model

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
        func =
            case state of
                NoState od ->
                    setState (initEdit od)

                Edit unsafeTitle od ->
                    let
                        tod =
                            odSetTitle (String.trim unsafeTitle) od
                    in
                    case removeBlankLeaf tod of
                        Just nod ->
                            setNoState nod

                        Nothing ->
                            stepSetState
                                (tod
                                    |> addNew
                                    |> Random.map initEdit
                                )
    in
    ( func model
    , focusPrimary
    )


initEdit : OD -> State
initEdit od =
    Edit (odTitle od) od


setQ : String -> Model -> Model
setQ nqs (Model s _ seed) =
    Model s nqs seed


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
    Sub.batch
        [ Browser.Events.onKeyDown
            (KeyEvent.decoder |> JD.andThen (condAlways keyMap.global >> failWhenNothing))
        ]


failWhenNothing : Maybe a -> Decoder a
failWhenNothing =
    Maybe.map JD.succeed
        >> Maybe.withDefault (JD.fail "not interested")



-- VIEW


type alias HM =
    Html Msg


type alias LHM =
    List (Html Msg)


view : Model -> Html Msg
view (Model state qs _) =
    div []
        [ div [ class "center measure-wide" ]
            [ button [ onClick Download, accesskey 's' ]
                [ i [ class "material-icons" ] [ text "save_alt" ] ]
            , button [ onClick Upload, accesskey 'o' ]
                [ i [ class "material-icons" ] [ text "folder_open" ] ]
            , div [ class "pa1 f4 lh-title" ] [ text "OZ OUTLINING V2" ]
            , viewSearchQuery qs
            , viewOD qs state
            ]
        ]


type alias KeyMap =
    { global : List ( KeyEvent -> Bool, Msg )
    , query : List ( KeyEvent -> Bool, Msg )
    , edit : List ( KeyEvent -> Bool, Msg )
    , focused : List ( KeyEvent -> Bool, Msg )
    }


keyMap : KeyMap
keyMap =
    let
        key =
            KeyEvent.hot

        shift =
            KeyEvent.shift

        any =
            anyPass

        enter =
            key "Enter"

        shiftEnter =
            shift "Enter"

        tab =
            key "Tab"

        shiftTab =
            shift "Tab"

        aUp =
            "ArrowUp"

        aDown =
            "ArrowDown"

        aLeft =
            "ArrowLeft"

        aRight =
            "ArrowRight"

        up =
            key aUp

        down =
            key aDown

        left =
            key aLeft

        right =
            key aRight

        shiftRight =
            shift aRight

        shiftLeft =
            shift aLeft

        esc =
            key "Escape"

        ctrl =
            KeyEvent.ctrl
    in
    { global =
        [ ( ctrl "o", Upload )
        , ( ctrl "s", Download )
        ]
    , query =
        [ ( enter, OnQueryEnter )
        , ( shiftEnter, OnQueryShiftEnter )
        , ( esc, OnEscQ )
        ]
    , edit =
        [ ( enter, OnEnter )
        , ( esc, OnEsc )
        , ( tab, Indent )
        , ( shiftTab, UnIndent )
        ]
    , focused =
        [ ( enter, OnEnter )
        , ( esc, OnEsc )
        , ( any [ up, key "k" ], OnCursorUp )
        , ( any [ ctrl aUp, ctrl "k" ], OnCursorUpRelocate )
        , ( any [ down, key "j" ], OnCursorDown )
        , ( any [ ctrl aDown, ctrl "j" ], OnCursorDownRelocate )
        , ( any [ left, key "h" ], OnCursorLeft )
        , ( any [ ctrl aLeft, ctrl "h" ], UnIndent )
        , ( any [ right, key "l" ], OnCursorRight )
        , ( any [ ctrl aRight, ctrl "l" ], Indent )
        , ( tab, Indent )
        , ( shiftTab, UnIndent )
        , ( key "n", SearchForward )
        , ( shift "N", SearchBackward )
        , ( shiftRight, ZoomIn )
        , ( shiftLeft, ZoomOut )
        , ( key "/", FocusSearch )
        ]
    }


viewSearchQuery : String -> HM
viewSearchQuery qs =
    div [ class "pv1 flex" ]
        [ input
            [ Html.Attributes.id "search-input"
            , class "flex-auto"
            , value qs
            , onInput QueryChanged
            , placeholder "Search..."
            , onKeyDownHelp keyMap.query
            ]
            []
        ]



-- OD VM View


type CollapseState
    = NoChildren
    | Collapsed
    | Expanded


type IV
    = IVEdit String
    | IVShow Id String
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
                    ( iv, NoChildren, [] )

                TVCollapsed iv ->
                    ( iv, Collapsed, [] )

                TVExpanded iv tvs ->
                    ( iv, Expanded, tvs )

        iconName =
            case collapseState of
                NoChildren ->
                    "chevron_right"

                Expanded ->
                    "expand_more"

                Collapsed ->
                    "chevron_right"
    in
    treeContainer
        [ div [ class "flex" ]
            [ i
                [ class "self-start pt2 ph1 material-icons md-24 light-silver"
                , classIf (collapseState == NoChildren) "o-0"
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
                , class "flex-auto pa1 bg-light-green bn bw0 ma0"
                , tabindex 0
                , value title
                , onInput TitleChanged
                , onKeyDownHelp keyMap.edit
                ]
                []

        IVShow id title ->
            div
                [ class "flex-auto pa1"
                , onClick (IdClicked id)
                ]
                [ displayTitleEl title ]

        IVFocused title ->
            div
                [ Html.Attributes.id "primary-focus-node"
                , class "flex-auto pa1 bg-lightest-blue"
                , tabindex 0
                , onKeyDownHelp keyMap.focused
                , onClick OnEnter
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
addNewHelp id ((OD pcs cs (LTR l t r)) as od) =
    let
        newT =
            treeFromId id
    in
    if isExpandedAndWithChildren od then
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


findId : Id -> OD -> Maybe OD
findId id =
    findFirst (propEq odId id)


findNextWrap : (OD -> Bool) -> OD -> Maybe OD
findNextWrap pred =
    firstOf
        [ findNext pred
        , findFirst pred
        ]


findPrevWrap : (OD -> Bool) -> OD -> Maybe OD
findPrevWrap pred =
    firstOf
        [ findPrev pred
        , findLast pred
        ]


findFirst : (OD -> Bool) -> OD -> Maybe OD
findFirst pred =
    firstRoot >> findI next pred


findLast : (OD -> Bool) -> OD -> Maybe OD
findLast pred =
    lastRoot >> applyWhileJust lastChild >> findI prev pred


findNext : (OD -> Bool) -> OD -> Maybe OD
findNext =
    findX next


findPrev : (OD -> Bool) -> OD -> Maybe OD
findPrev =
    findX prev


next : OD -> Maybe OD
next =
    firstOf [ firstChild, tryRight, tryRightOfAncestor ]


prev : OD -> Maybe OD
prev =
    firstOf
        [ tryLeft >> Maybe.map (applyWhileJust lastChild)
        , parent
        ]


tryForward : OD -> Maybe OD
tryForward =
    firstOf
        [ expandedAndWithChildren >> Maybe.andThen firstChild
        , tryRight
        , tryRightOfAncestor
        ]


tryBackward : OD -> Maybe OD
tryBackward =
    firstOf
        [ tryLeft
            >> Maybe.map
                (applyWhileJust
                    (expandedAndWithChildren >> Maybe.andThen lastChild)
                )
        , parent
        ]


lastChild : OD -> Maybe OD
lastChild (OD pcs cs (LTR l (T i ts) r)) =
    case List.reverse ts of
        first :: rest ->
            LTR rest first []
                |> OD pcs (Crumb l i r :: cs)
                |> Just

        [] ->
            Nothing


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


parent : OD -> Maybe OD
parent (OD pcs cs (LTR l t r)) =
    case cs of
        (Crumb cl item cr) :: rest ->
            LTR cl (T item (List.reverse l ++ t :: r)) cr
                |> OD pcs rest
                |> Just

        [] ->
            Nothing


firstChild : OD -> Maybe OD
firstChild (OD pcs cs (LTR l (T item ts) r)) =
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
    applyWhileJust parent


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


relocateBefore : OD -> Maybe OD
relocateBefore (OD pcs cs (LTR l t r)) =
    case l of
        first :: rest ->
            LTR rest t (first :: r)
                |> OD pcs cs
                |> Just

        [] ->
            Nothing


relocateAfter : OD -> Maybe OD
relocateAfter (OD pcs cs (LTR l t r)) =
    case r of
        first :: rest ->
            LTR (first :: l) t rest
                |> OD pcs cs
                |> Just

        [] ->
            Nothing


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


removeBlankLeaf : OD -> Maybe OD
removeBlankLeaf ((OD _ _ (LTR _ (T (Item _ _ title) ts) _)) as od) =
    if isBlank title && List.isEmpty ts then
        removeGoLeftOrRightOrUp od

    else
        Nothing


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
odToTVL iv (OD _ cs (LTR l (T (Item _ collapsed _) ts) r)) =
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
crumbToLVR (Crumb l (Item id _ title) r) lvr =
    LVR l (TVExpanded (IVShow id title) (lvrToTVL lvr)) r


lvrToTVL : LVR -> List TV
lvrToTVL (LVR l tv r) =
    (l |> List.reverse |> List.map toTV) ++ tv :: (r |> List.map toTV)


toTV : T -> TV
toTV (T (Item id collapsed title) ts) =
    let
        iv =
            IVShow id title
    in
    case ( ts, collapsed ) of
        ( [], _ ) ->
            TVLeaf iv

        ( _, True ) ->
            TVCollapsed iv

        ( _, False ) ->
            TVExpanded iv (List.map toTV ts)



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


isExpandedAndWithChildren : OD -> Bool
isExpandedAndWithChildren (OD _ _ (LTR _ (T i ts) _)) =
    not (List.isEmpty ts) && itemExpanded i


expandedAndWithChildren : OD -> Maybe OD
expandedAndWithChildren od =
    if isExpandedAndWithChildren od then
        Just od

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



-- Id


type alias Id =
    ItemId


idGen : Generator Id
idGen =
    ItemId.generator
