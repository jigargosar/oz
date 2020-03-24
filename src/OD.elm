module OD exposing (Item, OD, addNew, new, view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import ItemId exposing (ItemId)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Random exposing (Generator)
import Utils exposing (..)



-- OUTLINE DOC


type OD
    = OD (List Crumb) (List Crumb) LTR


encoder : OD -> Value
encoder (OD pcs cs (LTR l t r)) =
    JE.object
        [ ( "pcs", JE.list crumbEncoder pcs )
        , ( "cs", JE.list crumbEncoder cs )
        , ( "l", JE.list treeEncoder l )
        , ( "t", treeEncoder t )
        , ( "r", JE.list treeEncoder r )
        ]


decoder : Decoder OD
decoder =
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



-- VIEW


view : OD -> Html msg
view (OD _ _ (LTR l t r)) =
    div []
        (List.map viewTree (List.reverse l)
            ++ viewTree t
            :: List.map viewTree r
        )


viewTree : T -> Html msg
viewTree (T item ts) =
    div []
        [ div [] [ text (itemDisplayTitle item) ]
        , div [ class "pr3" ] (List.map viewTree ts)
        ]
