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
    = Model OD State Seed


type State
    = Edit Id String
    | NoEdit


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
            ( Model newOD NoEdit seed
            , Dom.focus "primary-focus-node" |> Task.attempt OnFocusResult
            )

        Ok (Just od) ->
            ( Model od NoEdit (Random.initialSeed flags.now)
            , Dom.focus "primary-focus-node" |> Task.attempt OnFocusResult
            )

        Err err ->
            Debug.todo (JD.errorToString err)



-- Update


type Msg
    = NoOp
    | OnFocusResult (Result Dom.Error ())
    | AddNew
    | StartEditTitle
    | TitleChanged String
    | SaveEditTitle


aroundUpdate : Msg -> Model -> ( Model, Cmd Msg )
aroundUpdate msg ((Model oldOd oldState _) as model) =
    let
        newModel =
            update msg model

        (Model newOd newState _) =
            newModel

        odChanged =
            oldOd /= newOd

        stateSwitched =
            (oldState /= newState)
                && (oldState == NoEdit || newState == NoEdit)
    in
    ( newModel
    , Cmd.batch
        [ cmdIf (odChanged || stateSwitched)
            (Dom.focus "primary-focus-node" |> Task.attempt OnFocusResult)
        , cmdIf odChanged (cacheODCmd newOd)
        ]
    )


cacheODCmd : OD -> Cmd msg
cacheODCmd od =
    cacheKV ( "od", odEncoder od )


update : Msg -> Model -> Model
update message ((Model od state seed) as model) =
    case message of
        NoOp ->
            model

        OnFocusResult (Ok ()) ->
            model

        OnFocusResult (Err (Dom.NotFound domId)) ->
            Debug.todo ("focus failed on: " ++ domId)

        AddNew ->
            Random.step (addNew od) seed
                |> uncurry (\newOd -> Model newOd state)

        StartEditTitle ->
            case ( state, itemOf od ) of
                ( NoEdit, Item id _ title ) ->
                    Model od (Edit id title) seed

                ( Edit _ _, _ ) ->
                    model

        TitleChanged changedTitle ->
            case state of
                Edit editId _ ->
                    let
                        newState =
                            if idOfOd od == editId then
                                Edit editId changedTitle

                            else
                                NoEdit
                    in
                    Model od newState seed

                NoEdit ->
                    model

        SaveEditTitle ->
            case state of
                Edit editId title ->
                    Model
                        (if idOfOd od == editId then
                            odSetTitle title od

                         else
                            od
                        )
                        NoEdit
                        seed

                NoEdit ->
                    model


itemOf : OD -> Item
itemOf (OD _ _ (LTR _ (T item _) _)) =
    item


idOfOd : OD -> Id
idOfOd =
    itemOf >> idOf


odSetTitle : String -> OD -> OD
odSetTitle title (OD pcs cs (LTR l (T (Item id collapsed _) ts) r)) =
    OD pcs cs (LTR l (T (Item id collapsed title) ts) r)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- VIEW


view : Model -> Html Msg
view (Model od st _) =
    div []
        [ div [] [ text "OZ OUTLINING V2" ]
        , viewOD st od
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



-- OUTLINE DOC VIEW


viewOD : State -> OD -> Html Msg
viewOD st (OD _ _ (LTR l t r)) =
    div []
        (List.map (viewTree st False) (List.reverse l)
            ++ viewTree st True t
            :: List.map (viewTree st False) r
        )


viewTree : State -> Bool -> T -> Html Msg
viewTree st isHighlighted (T item ts) =
    div []
        [ case st of
            Edit editId editTitle ->
                if isHighlighted && editId == idOf item then
                    viewTitleEditor editTitle

                else
                    viewTitle isHighlighted item

            NoEdit ->
                viewTitle isHighlighted item
        , div [ class "pr3" ] (List.map (viewTree st False) ts)
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
                [ ( KeyEvent.hot "Enter", SaveEditTitle ) ]
            ]
            []
        ]


viewTitle : Bool -> Item -> Html Msg
viewTitle isHighlighted item =
    div
        (if isHighlighted then
            [ Html.Attributes.id "primary-focus-node"
            , tabindex 0
            , onKeyDownHelp
                [ ( KeyEvent.hot "Enter", StartEditTitle ) ]
            ]

         else
            []
        )
        [ text (itemDisplayTitle item) ]


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


idOf : Item -> Id
idOf (Item id _ _) =
    id


itemFromId : Id -> Item
itemFromId id =
    Item id False ""


itemCollapsed : Item -> Bool
itemCollapsed (Item _ c _) =
    c


itemExpanded : Item -> Bool
itemExpanded =
    itemCollapsed >> not


itemDisplayTitle : Item -> String
itemDisplayTitle (Item _ _ ti) =
    case nonBlank ti of
        Just title ->
            title

        Nothing ->
            "Untitled"



-- Id


type alias Id =
    ItemId


idGen : Generator Id
idGen =
    ItemId.generator
