port module OD exposing (main)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, tabindex)
import Html.Events
import ItemId exposing (ItemId)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import KeyEvent
import Random exposing (Generator, Seed)
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
    = Model OD Seed


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
            ( Model newOD seed
            , Cmd.none
            )

        Ok (Just od) ->
            ( Model od (Random.initialSeed flags.now)
            , Cmd.none
            )

        Err err ->
            Debug.todo (JD.errorToString err)



-- Update


type Msg
    = NoOp
    | AddNew


aroundUpdate : Msg -> Model -> ( Model, Cmd Msg )
aroundUpdate msg model =
    let
        newModel =
            update msg model

        (Model od _) =
            newModel
    in
    ( newModel, cacheODCmd od )


cacheODCmd : OD -> Cmd msg
cacheODCmd od =
    cacheKV ( "od", odEncoder od )


update : Msg -> Model -> Model
update message ((Model od seed) as model) =
    case message of
        NoOp ->
            model

        AddNew ->
            Random.step (addNew od) seed
                |> uncurry Model


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- VIEW


view : Model -> Html Msg
view (Model od _) =
    div []
        [ div [] [ text "OZ OUTLINING V2" ]
        , viewOD od
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
    if treeHasExpandedChildren t then
        -- insertAfter
        OD pcs cs (LTR (t :: l) newT r)

    else
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



-- OUTLINE DOC VIEW


viewOD : OD -> Html Msg
viewOD (OD _ _ (LTR l t r)) =
    div []
        (List.map viewTree (List.reverse l)
            ++ viewTree t
            :: List.map viewTree r
        )


viewTree : T -> Html Msg
viewTree (T item ts) =
    div []
        [ div
            [ tabindex 0
            , Html.Events.preventDefaultOn "keydown"
                (KeyEvent.decoder
                    |> JD.andThen
                        (\ke ->
                            condAlways [ ( KeyEvent.hot "Enter", AddNew ) ] ke
                                |> Maybe.map (\msg -> JD.succeed ( msg, True ))
                                |> Maybe.withDefault (JD.fail "Not interested")
                        )
                )
            ]
            [ text (itemDisplayTitle item) ]
        , div [ class "pr3" ] (List.map viewTree ts)
        ]



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


treeHasExpandedChildren : T -> Bool
treeHasExpandedChildren (T item ts) =
    List.isEmpty ts || itemCollapsed item



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
