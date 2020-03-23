module OutlineDoc.FIZ exposing
    ( FIZ
    , Item
    , newLeaf
    )

import Forest.Tree as Tree exposing (Forest, Tree)
import Forest.Zipper exposing (ForestZipper, Location)
import ItemId exposing (ItemId)
import Json.Decode as JD exposing (Decoder)
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
    ItemId.generator
        |> Random.map (\id -> { id = id, title = title, collapsed = False })



-- MODEL


type alias FIZ =
    ForestZipper Item


newLeaf : Generator (Tree Item)
newLeaf =
    let
        itemToTree : Item -> Tree Item
        itemToTree item =
            Tree.tree item []
    in
    itemGenerator "" |> Random.map itemToTree
