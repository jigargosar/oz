module OutlineDoc.LineZipper exposing (LineZipper, decoder, encoder)

import Forest.Tree as T exposing (Forest, Tree)
import Forest.Zipper as Z exposing (ForestZipper, Location)
import ItemId exposing (ItemId)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import List.Extra
import Random exposing (Generator)
import Utils exposing (..)



-- ITEM


type alias Item =
    { id : ItemId
    , title : String
    , collapsed : Bool
    }


strId =
    .id >> ItemId.toString


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
            T.tree item []
    in
    itemGenerator "" |> Random.map itemToTree



-- MODEL


type LineZipper
    = LineZipper (ForestZipper Item)



-- Check for invariants before wrapping


unwrap : LineZipper -> ForestZipper Item
unwrap (LineZipper z) =
    let
        _ =
            validate z
                |> Result.mapError Debug.todo
    in
    z


wrap : ForestZipper Item -> LineZipper
wrap z =
    let
        _ =
            validate z
                |> Result.mapError Debug.todo
    in
    LineZipper z


validate : ForestZipper Item -> Result String (ForestZipper Item)
validate z =
    if hasCollapsedAncestors z then
        Err "hasCollapsedAncestors"

    else if hasDuplicateItemIds z then
        Err "hasDuplicateItemIds"

    else
        Ok z


hasCollapsedAncestors : ForestZipper Item -> Bool
hasCollapsedAncestors =
    Z.ancestors >> List.map .collapsed >> List.any identity


hasDuplicateItemIds : ForestZipper { a | id : ItemId } -> Bool
hasDuplicateItemIds =
    Z.foldl (::) [] >> List.Extra.allDifferentBy strId >> not



-- JSON


encoder : LineZipper -> Value
encoder =
    unwrap >> Z.encoder itemEncoder


decoder : Decoder LineZipper
decoder =
    Z.decoder itemDecoder |> JD.map wrap
