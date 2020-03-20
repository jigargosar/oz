module FIZ exposing
    ( FIZ
    , addNew
    , decoder
    , encoder
    , new
    , restructureCursorWithContext
    , restructureWithContext
    )

import CollapseState exposing (CollapseState)
import Forest.Tree as Tree exposing (Forest, Tree)
import Forest.Zipper as Zipper exposing (ForestZipper, Location)
import ItemId exposing (ItemId)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Random exposing (Generator)
import Utils exposing (..)



-- ITEM


type alias Item =
    { id : ItemId
    , title : String
    , collapsed : Bool
    }


itemGenerator : String -> Generator Item
itemGenerator title =
    ItemId.itemIdGenerator
        |> Random.map (\id -> { id = id, title = title, collapsed = False })


itemEncoder : Item -> Value
itemEncoder item =
    JE.object
        [ ( "id", ItemId.itemIdEncoder item.id )
        , ( "title", JE.string item.title )
        , ( "collapsed", JE.bool item.collapsed )
        ]


itemDecoder : Decoder Item
itemDecoder =
    JD.succeed Item
        |> required "id" ItemId.itemIdDecoder
        |> required "title" JD.string
        |> JD.map2 (|>) (JD.oneOf [ JD.field "collapsed" JD.bool, JD.succeed False ])



-- MODEL


type alias FIZ =
    ForestZipper Item


encoder : FIZ -> Value
encoder zipper =
    Zipper.encoder itemEncoder zipper


decoder : Decoder FIZ
decoder =
    Zipper.decoder itemDecoder


emptyLeafGenerator : Generator (Tree Item)
emptyLeafGenerator =
    let
        itemToTree : Item -> Tree Item
        itemToTree item =
            Tree.tree item []
    in
    itemGenerator "" |> Random.map itemToTree


new : Generator FIZ
new =
    emptyLeafGenerator |> Random.map Zipper.fromTree


hasVisibleChildren : FIZ -> Bool
hasVisibleChildren fiz =
    not (Zipper.isLeaf fiz || (Zipper.data fiz |> .collapsed))



-- NEW INSERTIONS


addNew : FIZ -> Generator FIZ
addNew fiz =
    let
        insertNewHelper node =
            if hasVisibleChildren fiz then
                Zipper.prependChildGo node fiz

            else
                Zipper.insertRightGo node fiz
    in
    emptyLeafGenerator
        |> Random.map insertNewHelper



-- VIEW HELPERS


restructureWithContext : (Item -> List Item -> CollapseState -> List b -> b) -> ForestZipper Item -> List b
restructureWithContext render =
    Zipper.restructure
        (\fiz children ->
            let
                item =
                    Zipper.data fiz
            in
            render
                item
                (Zipper.ancestors fiz)
                (if List.isEmpty children then
                    CollapseState.NoChildren

                 else if item.collapsed then
                    CollapseState.Collapsed

                 else
                    CollapseState.Expanded
                )
                (if item.collapsed then
                    []

                 else
                    children
                )
        )


restructureCursorWithContext : (Item -> List Item -> CollapseState -> List b -> b) -> ForestZipper Item -> List b
restructureCursorWithContext render =
    Zipper.tree
        >> Zipper.fromTree
        >> restructureWithContext render
