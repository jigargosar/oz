module OutlineDoc.LineZipper exposing (LineZipper, decoder, encoder)

import Forest.Tree as Tree exposing (Forest, Tree)
import Forest.Zipper as Z exposing (ForestZipper, Location)
import ItemId exposing (ItemId)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Random exposing (Generator)
import Utils exposing (..)


type alias Item =
    { id : ItemId
    , title : String
    , collapsed : Bool
    }


itemGenerator : String -> Generator Item
itemGenerator title =
    ItemId.generator
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


newLeaf : Generator (Tree Item)
newLeaf =
    let
        itemToTree : Item -> Tree Item
        itemToTree item =
            Tree.tree item []
    in
    itemGenerator "" |> Random.map itemToTree


type LineZipper
    = LineZipper (ForestZipper Item)


encoder : LineZipper -> Value
encoder (LineZipper lz) =
    Z.encoder itemEncoder lz


decoder : Decoder LineZipper
decoder =
    Z.decoder itemDecoder |> JD.map LineZipper
