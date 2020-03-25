port module OD exposing (main)

import Browser
import Browser.Dom as Dom
import Html exposing (Html, div, input, text)
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
    | Indent
    | UnIndent


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
                                    model

        TitleChanged changedTitle ->
            case state of
                Edit _ od ->
                    Model (Edit changedTitle od) seed

                NoEdit _ ->
                    model

        OnCursorUp ->
            case state of
                NoEdit od ->
                    let
                        tryLeft (OD pcs cs (LTR l t r)) =
                            case l of
                                first :: rest ->
                                    LTR rest first (t :: r)
                                        |> OD pcs cs
                                        |> Just

                                [] ->
                                    Nothing

                        tryUp (OD pcs cs (LTR l t r)) =
                            case cs of
                                (Crumb cl item cr) :: rest ->
                                    LTR cl (T item (List.reverse l ++ t :: r)) cr
                                        |> OD pcs rest
                                        |> Just

                                [] ->
                                    Nothing
                    in
                    case firstOf [ tryLeft, tryUp ] od of
                        Just newOD ->
                            Model (NoEdit newOD) seed

                        Nothing ->
                            model

                _ ->
                    model

        OnCursorDown ->
            case state of
                NoEdit od ->
                    let
                        tryRight (OD pcs cs (LTR l t r)) =
                            case r of
                                first :: rest ->
                                    LTR (t :: l) first rest
                                        |> OD pcs cs
                                        |> Just

                                [] ->
                                    Nothing
                    in
                    case firstOf [ tryRight ] od of
                        Just newOD ->
                            Model (NoEdit newOD) seed

                        Nothing ->
                            model

                _ ->
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
            model


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


view : Model -> Html Msg
view (Model state _) =
    div []
        [ div [] [ text "OZ OUTLINING V2" ]
        , viewOD state
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
            Just (OD pcs (Crumb rest item r :: cs) (LTR (List.reverse ts) t []))


removeLeaf : OD -> Maybe OD
removeLeaf ((OD _ _ (LTR _ t _)) as od) =
    if hasChildren t then
        Nothing

    else
        removeGoLeftRightOrUp od


removeGoLeftRightOrUp : OD -> Maybe OD
removeGoLeftRightOrUp (OD pcs cs (LTR l _ r)) =
    case ( l, r, cs ) of
        ( first :: rest, _, _ ) ->
            Just (OD pcs cs (LTR rest first r))

        ( _, first :: rest, _ ) ->
            Just (OD pcs cs (LTR l first rest))

        ( _, _, (Crumb crL item crR) :: rest ) ->
            Just (OD pcs rest (LTR crL (T item []) crR))

        _ ->
            Nothing



-- OUTLINE DOC VIEW


viewOD : State -> Html Msg
viewOD state =
    case state of
        Edit title (OD _ cs (LTR l t r)) ->
            viewCrumbs cs
                (List.map viewBasicTree (List.reverse l)
                    ++ viewTitleEditorTree title t
                    :: List.map viewBasicTree r
                )

        NoEdit (OD _ cs (LTR l t r)) ->
            viewCrumbs cs
                (List.map viewBasicTree (List.reverse l)
                    ++ viewFocusedTree t
                    :: List.map viewBasicTree r
                )


viewCrumbs : List Crumb -> LHM -> HM
viewCrumbs cs lhm =
    case cs of
        [] ->
            treeChildrenContainer lhm

        (Crumb l item r) :: rest ->
            viewCrumbs rest
                (List.map viewBasicTree (List.reverse l)
                    ++ treeContainer [ viewBasicTitle item, treeChildrenContainer lhm ]
                    :: List.map viewBasicTree r
                )


type alias HM =
    Html Msg


type alias LHM =
    List (Html Msg)


treeChildrenContainer : LHM -> HM
treeChildrenContainer =
    div [ class "pl3" ]


treeContainer : LHM -> HM
treeContainer =
    div []


viewFocusedTree : T -> Html Msg
viewFocusedTree (T (Item _ _ title) ts) =
    treeContainer
        [ viewFocusedTitle title
        , treeChildrenContainer (List.map viewBasicTree ts)
        ]


viewFocusedTitle : String -> Html Msg
viewFocusedTitle title =
    div
        [ Html.Attributes.id "primary-focus-node"
        , tabindex 0
        , onKeyDownHelp
            [ ( KeyEvent.hot "Enter", OnEnter )
            , ( KeyEvent.hot "ArrowUp", OnCursorUp )
            , ( KeyEvent.hot "ArrowDown", OnCursorDown )
            , ( KeyEvent.hot "Tab", Indent )
            , ( KeyEvent.shift "Tab", UnIndent )
            ]
        ]
        [ text (displayTitle title) ]


viewTitleEditorTree : String -> T -> Html Msg
viewTitleEditorTree title (T _ ts) =
    treeContainer
        [ viewTitleEditor title
        , treeChildrenContainer (List.map viewBasicTree ts)
        ]


viewTitleEditor : String -> Html Msg
viewTitleEditor title =
    div []
        [ input
            [ Html.Attributes.id "primary-focus-node"
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
        ]


viewBasicTree : T -> Html Msg
viewBasicTree (T item ts) =
    treeContainer
        [ viewBasicTitle item
        , treeChildrenContainer (List.map viewBasicTree ts)
        ]


viewBasicTitle : Item -> HM
viewBasicTitle (Item _ _ title) =
    div [] [ text (displayTitle title) ]


displayTitle : String -> String
displayTitle =
    nonBlank >> Maybe.withDefault "Untitled"


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
